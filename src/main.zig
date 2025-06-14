const std = @import("std");
// const print = std.debug.print;
const String = []const u8;
const lib = @import("lib.zig");

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();

    // Выделяем буфер для чтения
    var buffer: [4096]u8 = undefined;

    while (true) {
        // Выводим приглашение
        try stdout.print("Введите текст (или 'quit' для выхода): ", .{});

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
