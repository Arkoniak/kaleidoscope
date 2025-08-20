const std = @import("std");
const lexer = @import("lexer.zig");
const ast = @import("ast.zig");
const parser_module = @import("parser.zig");
const Parser = @import("parser.zig").Parser;
const testing = std.testing;

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var buffer: [4096]u8 = undefined;

    while (true) {
        defer _ = arena.reset(.retain_capacity);
        try stdout.print("> ", .{});

        if (try stdin.readUntilDelimiterOrEof(&buffer, '\n')) |input| {
            var parser = Parser.create(input, allocator);
            const expr = try parser.parse() orelse continue;
            try stdout.print("{}\n", .{expr});
        } else {
            break;
        }
    }
}

test "Lexer" {
    _ = lexer;
}

test "AST" {
    _ = ast;
}

test "Parser" {
    _ = parser_module;
}
