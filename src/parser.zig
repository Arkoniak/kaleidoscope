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

const Precedence = enum(u8) {
    wrong = 0,
    lowest = 1,
    less = 10,
    plus = 20,
    multiplication = 40,
};

fn binopPrecedence(tag: lexer.Tag) Precedence {
    return switch(tag) {
        .lt => .less,
        .gt => .less,
        .minus => .plus,
        .plus  => .plus,
        .multiply => .multiplication,
        .divide   => .multiplication,
        else => .wrong,
    };
}

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
            result = switch(self.token.tag) {
                .def => try self.handleDefinition(),
                .tag_extern => try self.handleExtern(),
                else => try self.handleTopLevelExpr(),
            };
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

    fn handleDefinition(self: *Self) ParserError!?*ast.Expr {
        const expr = try self.parseDefinition();
        if (expr == null) {
            self.nextToken();
        }

        return expr;
    }

    fn handleExtern(self: *Self) ParserError!?*ast.Expr {
        const expr = try self.parseExternal();
        if (expr == null) {
            self.nextToken();
        }

        return expr;
    }

    /// toplevelexpr ::= expression
    fn parseTopLevelExpr(self: *Self) !?*ast.Expr {
        const expr = try self.parseExpr() orelse return null;
        const args = std.ArrayList([]const u8).init(self.allocator);
        const proto = try ast.Expr.prototype(self.allocator, "__anon_expr", args.items);
        return try ast.Expr.function(self.allocator, proto, expr);
    }

    /// expression
    ///   ::= primary binoprhs
    ///
    fn parseExpr(self: *Self) ParserError!?*ast.Expr {
        const lhs = try self.parsePrimary() orelse return null;

        return self.parseBinOpExprRHS(@intFromEnum(Precedence.lowest), lhs);
    }

    /// binoprhs
    ///   ::= ('+' primary)*
    fn parseBinOpExprRHS(self: *Self, exprPrec: u8, lhs: *ast.Expr) ParserError!?*ast.Expr {
        var lhs1 = lhs;
        while(true) {
            const tokPrec: u8 = @intFromEnum(binopPrecedence(self.token.tag));
            if (tokPrec < exprPrec) {
                return lhs1;
            }
            
            const binop = self.token.tag;
            self.nextToken();

            var rhs = try self.parsePrimary() orelse return null;
            
            const nextPrec: u8 = @intFromEnum(binopPrecedence(self.token.tag));
            if (tokPrec < nextPrec) {
                rhs = try self.parseBinOpExprRHS(tokPrec + 1, rhs) orelse return null;
            }
            lhs1 = try ast.Expr.binop(self.allocator, binop, lhs1, rhs);
        }
    }

    /// primary
    ///   ::= identifierexpr
    ///   ::= numberexpr
    ///   ::= parenexpr
    fn parsePrimary(self: *Self) ParserError!?*ast.Expr {
        switch(self.token.tag) {
            .number => {
                return try self.parseNumberExpr();
            },
            .identifier => {
                return try self.parseIdentifierExpr();
            },
            .lparen => {
                return try self.parseParenExpr();
            },
            else => return null,
        }
    }

    /// parenexpr ::= '(' expression ')'
    fn parseParenExpr(self: *Self) ParserError!?*ast.Expr {
        self.nextToken();
        const expr = try self.parseExpr() orelse return null;
        if (self.token.tag != lexer.Tag.rparen) {
            // TODO: should be error
            return null;
        }
        self.nextToken();

        return expr;
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

        if (self.token.tag != .lparen) {
            const var_expr = try ast.Expr.variable(self.allocator, ident);
            return var_expr;
        }
        var args = std.ArrayList(*ast.Expr).init(self.allocator);
        while (true) {
            self.nextToken();
            if (self.token.tag == .rparen) {
                break;
            }
            const arg = try self.parseExpr() orelse return null;
            try args.append(arg);

            if (self.token.tag == .rparen) {
                break;
            }
            if (self.token.tag != .comma) {
                return null;
            }
        }
        self.nextToken();
        const call_expr = try ast.Expr.call(self.allocator, ident, args.items);
        return call_expr;
    }

    /// prototype
    ///   ::= id '(' id* ')'
    fn parsePrototypeExpr(self: *Self) ParserError!?*ast.Expr {
        const fname = self.lexer.inspect(self.token);
        self.nextToken();

        if (self.token.tag != .lparen) {
            // TODO: this should return an error
            return null;
        }
        self.nextToken();

        var args = std.ArrayList([]const u8).init(self.allocator);
        while (self.token.tag == .identifier) {
            const arg_name = self.lexer.inspect(self.token);
            try args.append(arg_name);
            self.nextToken();
        }
        if (self.token.tag != .rparen) {
            // TODO: this should return an error
            return null;
        }
        self.nextToken();

        const proto_expr = try ast.Expr.prototype(self.allocator, fname, args.items);
        return proto_expr;
    }

    /// definition ::= 'def' prototype expression
    fn parseDefinition(self: *Self) ParserError!?*ast.Expr {
        self.nextToken();
        const proto_expr = try self.parsePrototypeExpr() orelse return null;
        const expr = try self.parseExpr() orelse return null;
        const func_expr = try ast.Expr.function(self.allocator, proto_expr, expr);

        return func_expr;
    }

    /// external ::= 'extern' prototype
    fn parseExternal(self: *Self) ParserError!?*ast.Expr {
        self.nextToken();
        const proto_expr = try self.parsePrototypeExpr() orelse return null;
        return proto_expr;
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
    try testing.expectEqualStrings(stream.getWritten(), "__anon_expr() {\n    ret 5\n}");
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
    try testing.expectEqualStrings(stream.getWritten(), "__anon_expr() {\n    ret foo\n}");
}

test "Parsing expression in parenthesis" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const program = "(foo)";

    var parser = Parser.create(program, allocator);
    const expr = try parser.parse() orelse unreachable;

    var buffer: [4096]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buffer);
    const writer = stream.writer();

    try writer.print("{}", .{expr});
    try testing.expectEqualStrings(stream.getWritten(), "__anon_expr() {\n    ret foo\n}");
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
    try testing.expectEqualStrings(stream.getWritten(), "__anon_expr() {\n    ret foo(x, y)\n}");
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
    try testing.expectEqualStrings(stream.getWritten(), "__anon_expr() {\n    ret foo(x, y)\n}");
}

test "Parsing operations with precedence" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const program = "x + y";

    var parser = Parser.create(program, allocator);
    const expr = try parser.parse() orelse unreachable;

    var buffer: [4096]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buffer);
    const writer = stream.writer();

    try writer.print("{}", .{expr});
    try testing.expectEqualStrings(stream.getWritten(), "__anon_expr() {\n    ret (x + y)\n}");
}

test "Parsing nested operations with precedence" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const program = "2 + (3 + 4) * 5";

    var parser = Parser.create(program, allocator);
    const expr = try parser.parse() orelse unreachable;

    var buffer: [4096]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buffer);
    const writer = stream.writer();

    try writer.print("{}", .{expr});
    try testing.expectEqualStrings(stream.getWritten(), "__anon_expr() {\n    ret (2 + ((3 + 4) * 5))\n}");
}

test "Parsing function definition" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const program = "def foo(x y) bar(x) + baz(y)";

    var parser = Parser.create(program, allocator);
    const expr = try parser.parse() orelse unreachable;

    var buffer: [4096]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buffer);
    const writer = stream.writer();

    try writer.print("{}", .{expr});
    try testing.expectEqualStrings(stream.getWritten(), "foo(x y) {\n    ret (bar(x) + baz(y))\n}");
}
