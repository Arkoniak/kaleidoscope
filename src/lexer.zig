const std = @import("std");
const testing = std.testing;

// The lexer returns tokens [0-255] if it is an unknown character, otherwise one
// of these for known things.
const Tag = enum {
    eof,

    // commands
    def,
    tag_extern,

    //primary
    identifier,
    number,

    regular,
};

const Token = struct {
    s: u16,
    e: u16,
    tag: Tag,
};

fn isspace(c: u8) bool {
    return ((c == ' ') or (c == '\t'));
}

fn isalpha(c: u8) bool {
    return (((c >= 'a') and (c <= 'z')) or ((c >= 'A') and (c <= 'Z')));
}

fn isdigit(c: u8) bool {
    return ((c >= '0') and (c <= '9'));
}

fn isnum(c: u8) bool {
    return (isdigit(c) or c == '.');
}

fn isalphanum(c: u8) bool {
    return (isalpha(c) or isdigit(c));
}

pub fn gettok(buffer: []const u8, ptr0: u16) Token {
    var ptr = ptr0;
    const len: u16 = @intCast(buffer.len);
    if (ptr >= len) return Token{ .s = ptr0, .e = ptr, .tag = .eof };
    var lastchar = buffer[ptr];

    while (isspace(lastchar)) {
        ptr += 1;
        if (ptr < buffer.len) {
            lastchar = buffer[ptr];
        } else {
            return Token{ .s = ptr0, .e = ptr, .tag = .eof };
        }
    }

    if (isalpha(lastchar)) {
        while (isalphanum(lastchar)) {
            ptr += 1;
            if (ptr >= len) {
                ptr = len;
                break;
            }
            lastchar = buffer[ptr];
        }

        if (std.mem.eql(u8, buffer[ptr0..ptr], "def")) return Token{ .s = ptr0, .e = ptr, .tag = .def };
        if (std.mem.eql(u8, buffer[ptr0..ptr], "extern")) return Token{ .s = ptr0, .e = ptr, .tag = .tag_extern };
        return Token{ .s = ptr0, .e = ptr, .tag = .identifier };
    }

    if (isnum(lastchar)) {
        while (isnum(lastchar)) {
            ptr += 1;
            if (ptr >= len) {
                ptr = len;
                break;
            }
            lastchar = buffer[ptr];
        }

        return Token{ .s = ptr0, .e = ptr, .tag = .number };
    }

    if (lastchar == '#') {
        while (true) {
            ptr += 1;
            if ((lastchar == '\n') or (lastchar == '\r') or (ptr >= len)) break;
            lastchar = buffer[ptr];
        }

        if (ptr >= len) return Token{ .s = ptr0, .e = len - 1, .tag = .eof };
        return gettok(buffer, ptr);
    }

    return Token{ .s = ptr0, .e = ptr, .tag = .regular };
}

test "Def token" {
    const buffer = "def fib(x)";
    const token = gettok(buffer, 0);

    try testing.expect(token.tag == .def);
}

test "comment token" {
    const buffer = "#foo\n#bar";
    const token = gettok(buffer, 0);

    try testing.expect(token.tag == .eof);
}

test "alphanum token" {
    const buffer = "123.456 foo";
    const token = gettok(buffer, 0);

    try testing.expect(token.tag == .number);
}
