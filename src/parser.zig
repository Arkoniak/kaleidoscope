const std = @import("std");
const lexer = @import("lexer.zig");
const ast = @import("ast.zig");
const testing = std.testing;

const ParserError = error{
    // std.fmt.parseFloat errors
    InvalidCharacter,
    Overflow,
    
    // allocator.create errors
    OutOfMemory,
};

pub const Parser = struct {
    lexer: lexer.Lexer,
    allocator: std.mem.Allocator,
    token: lexer.Token,
    peek_token: lexer.Token,

    const Self = @This();
    pub fn create(buf: []const u8, allocator: std.mem.Allocator) Self {
        var lex = lexer.Lexer.create(buf);
        const token = lex.nextToken();
        const peek_token = lex.nextToken();
        const parser: Parser = .{.lexer = lex, .allocator = allocator, .token = token, .peek_token = peek_token};
        return parser;
    }

    fn nextToken(self: *Self) void {
        self.token = self.peek_token;
        self.peek_token = self.lexer.nextToken();
    }

    fn skipToken(self: *Self) void {
        self.peek_token = self.lexer.nextToken();
        self.token = self.peek_token;
        self.peek_token = self.lexer.nextToken();
    }

    pub fn parse(self: *Self) !?*ast.Expr {
        var result: ?*ast.Expr = null;
        while (self.token.tag != lexer.Tag.eof) {
            switch(self.token.tag) {
                else => result = try self.handleTopLevelExpr(),
            }
        }

        return result;
    }

    fn handleTopLevelExpr(self: *Self) !?*ast.Expr {
        const expr = try self.parseTopLevelExpr();
        if (expr == null) {
            self.nextToken();
        }
        return expr;
    }

    /// toplevelexpr ::= expression
    fn parseTopLevelExpr(self: *Self) !?*ast.Expr {
        const expr = try self.parseExpr() orelse return null;
        return try ast.Expr.function(self.allocator, expr);
    }

    /// expression
    ///   ::= primary binoprhs
    ///
    fn parseExpr(self: *Self) ParserError!?*ast.Expr {
        const lhs = try self.parsePrimary() orelse return null;

        return lhs;
    }

    /// primary
    ///   ::= identifierexpr
    ///   ::= numberexpr
    ///   ::= parenexpr
    fn parsePrimary(self: *Self) !?*ast.Expr {
        switch(self.token.tag) {
            lexer.Tag.number => {
                return try self.parseNumberExpr();
            },
            lexer.Tag.identifier => {
                return try self.parseIdentifierExpr();
            },
            else => return null,
        }
    }

    /// numberexpr ::= number
    fn parseNumberExpr(self: *Self) ParserError!?*ast.Expr {
        defer self.nextToken();
        const num_s = self.lexer.inspect(self.token);
        const num = try std.fmt.parseFloat(f64, num_s);
        const num_expr = try ast.Expr.number(self.allocator, num);
        return num_expr;
    }

    /// identifierexpr
    ///   ::= identifier
    ///   ::= identifier '(' expression* ')'
    fn parseIdentifierExpr(self: *Self) ParserError!?*ast.Expr {
        const ident = self.lexer.inspect(self.token);
        self.nextToken();

        if (self.token.tag != lexer.Tag.lparen) {
            const var_expr = try ast.Expr.variable(self.allocator, ident);
            return var_expr;
        }
        var args = std.ArrayList(*ast.Expr).init(self.allocator);
        while (true) {
            self.nextToken();
            if (self.token.tag == lexer.Tag.rparen) {
                break;
            }
            const arg = try self.parseExpr() orelse return null;
            try args.append(arg);

            if (self.token.tag == lexer.Tag.rparen) {
                break;
            }
            if (self.token.tag != lexer.Tag.comma) {
                return null;
            }
        }
        self.nextToken();
        const call_expr = try ast.Expr.call(self.allocator, ident, args.items);
        return call_expr;
    }
};

test "Lexer mutability inside parser" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const buffer = "def fib(x)";

    var parser = Parser.create(buffer, allocator);
    try testing.expectEqualStrings(parser.lexer.inspect(parser.token), "def");
}

test "Parsing number" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const program = "5";

    var parser = Parser.create(program, allocator);
    const expr = try parser.parse() orelse unreachable;

    var buffer: [4096]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buffer);
    const writer = stream.writer();

    try writer.print("{}", .{expr});
    try testing.expectEqualStrings(stream.getWritten(), "double _lambda() {\n    ret 5\n}");
}

test "Parsing variable" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const program = "foo";

    var parser = Parser.create(program, allocator);
    const expr = try parser.parse() orelse unreachable;

    var buffer: [4096]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buffer);
    const writer = stream.writer();

    try writer.print("{}", .{expr});
    try testing.expectEqualStrings(stream.getWritten(), "double _lambda() {\n    ret foo\n}");
}

test "Parsing call" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const program = "foo(x, y)";

    var parser = Parser.create(program, allocator);
    const expr = try parser.parse() orelse unreachable;

    var buffer: [4096]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buffer);
    const writer = stream.writer();

    try writer.print("{}", .{expr});
    try testing.expectEqualStrings(stream.getWritten(), "double _lambda() {\n    ret foo(x, y)\n}");
}

test "Parsing call with trailing commas" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const program = "foo(x, y, )";

    var parser = Parser.create(program, allocator);
    const expr = try parser.parse() orelse unreachable;

    var buffer: [4096]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buffer);
    const writer = stream.writer();

    try writer.print("{}", .{expr});
    try testing.expectEqualStrings(stream.getWritten(), "double _lambda() {\n    ret foo(x, y)\n}");
}

