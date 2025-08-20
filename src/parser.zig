const std = @import("std");
const lexer = @import("lexer.zig");
const ast = @import("ast.zig");
const testing = std.testing;

pub const Parser = struct {
    lexer: lexer.Lexer,
    allocator: std.mem.Allocator,
    token: lexer.Token = undefined,

    const Self = @This();
    pub fn create(buf: []const u8, allocator: std.mem.Allocator) Self {
        const lex = lexer.Lexer.create(buf);
        var parser: Parser = .{.lexer = lex, .allocator = allocator};
        parser.token = parser.nextToken();
        return parser;
    }

    fn nextToken(self: *Self) lexer.Token {
        return self.lexer.next_token();
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
            self.token = self.nextToken();
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
    fn parseExpr(self: *Self) !?*ast.Expr {
        const lhs = try self.parsePrimary() orelse return null;

        return lhs;
    }

    /// primary
    ///   ::= identifierexpr
    ///   ::= numberexpr
    ///   ::= parenexpr
    fn parsePrimary(self: *Self) !?*ast.Expr {
        defer self.token = self.nextToken();
        switch(self.token.tag) {
            lexer.Tag.number => {
                const num_s = self.lexer.inspect(self.token);
                const num = try std.fmt.parseFloat(f64, num_s);
                const num_expr = ast.Expr.number(self.allocator, num);
                return num_expr;
            },
            else => return null,
        }
    }

    // identifierexpr
    //   ::= identifier
    //   ::= identifier '(' expression* ')'

    // fn parseIdentifierExpr(self: *Self) ?*ast.Expr {
    //
    // }
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

// test "Parsing variable" {
//     var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
//     defer arena.deinit();
//     const allocator = arena.allocator();
//     const program = "foo";
//
//     var parser = Parser.create(program, allocator);
//     const expr = parser.parse() orelse unreachable;
//
//     var buffer: [4096]u8 = undefined;
//     var stream = std.io.fixedBufferStream(&buffer);
//     const writer = stream.writer();
//
//     try writer.print("{}", .{expr});
//     try testing.expectEqualStrings(stream.getWritten(), "double _lambda() {\n    ret foo\n}");
// }
