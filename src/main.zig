const std = @import("std");
const testing = std.testing;
// const print = std.debug.print;
const String = []const u8;
const lexer = @import("lexer.zig");
const ast = @import("ast.zig");
const parser = @import("parser.zig");

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();

    var buffer: [4096]u8 = undefined;

    while (true) {
        try stdout.print("ready> ", .{});

        if (try stdin.readUntilDelimiterOrEof(&buffer, '\n')) |input| {
            const trimmed = std.mem.trim(u8, input, "\r\n");

            if (std.mem.eql(u8, trimmed, "quit")) {
                try stdout.print("Good bye\n", .{});
                break;
            }

            try stdout.print("{s}\n", .{trimmed});
        } else {
            break;
        }
    }
}

test "Lexer" {
    _ = lexer;
}

// test "AST" {
//     _ = ast;
// }
//
// test "Parser" {
//     _ = parser;
// }
