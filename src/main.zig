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

    // Выделяем буфер для чтения
    var buffer: [4096]u8 = undefined;

    while (true) {
        // Выводим приглашение
        try stdout.print("ready> ", .{});

        // Читаем строку из stdin
        if (try stdin.readUntilDelimiterOrEof(&buffer, '\n')) |input| {
            // Убираем символ новой строки в конце, если он есть
            const trimmed = std.mem.trim(u8, input, "\r\n");

            // Проверяем на команду выхода
            if (std.mem.eql(u8, trimmed, "quit")) {
                try stdout.print("До свидания!\n", .{});
                break;
            }

            // Эхо: выводим введенный текст
            try stdout.print("Вы ввели: {s}\n", .{trimmed});
        } else {
            // EOF достигнут
            break;
        }
    }

    // print("Hello world, 2 + 3 = {d}\n", .{lib.add(2, 3)});
}

test "Lexer" {
    _ = lexer;
}

test "AST" {
    _ = ast;
}

test "Parser" {
    _ = parser;
}
