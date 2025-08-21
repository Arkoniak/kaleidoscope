const std = @import("std");
const testing = std.testing;

// The lexer returns tokens [0-255] if it is an unknown character, otherwise one
// of these for known things.
pub const Tag = enum {
    eof,

    // commands
    def,
    tag_extern,

    // primary
    identifier,
    number,

    // helpers
    lparen,
    rparen,
    comma,
    lt,
    gt,
    minus,
    plus,
    multiply,
    divide,

    // everything else
    unknown,
};

pub const Token = struct {
    s: u16,
    e: u16,
    tag: Tag,
};

fn isspace(c: u8) bool {
    return ((c == ' ') or (c == '\t') or (c == '\n') or (c == '\r'));
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

fn isnewline(c: u8) bool {
    return ((c == '\n') or (c == '\r'));
}

pub const Lexer = struct {
    buf: []const u8,
    i: u16 = 0,
    char: u8 = 0,

    const Self = @This();
    pub fn create(buf: []const u8) Self {
        var lexer: Self = .{.buf = buf};
        if (buf.len > 0) {
            lexer.char = buf[0];
        }

        return lexer;
    }

    pub fn nextToken(self: *Self) Token {
        self.skipWhitechars();
        
        const token: Token = switch(self.char) {
            0 => .{.s = self.i, .e = self.i, .tag = .eof},
            '#' => cmt: {
                self.skipComment();
                break :cmt self.nextToken();
            },
            ')' => self.charToken(.rparen),
            '(' => self.charToken(.lparen),
            ',' => self.charToken(.comma),
            '<' => self.charToken(.lt),
            '>' => self.charToken(.gt),
            '+' => self.charToken(.plus),
            '-' => self.charToken(.minus),
            '*' => self.charToken(.multiply),
            '/' => self.charToken(.divide),
            else => blk: {
                if (isalpha(self.char)) {
                    const ident = self.read_identifier();
                    if (self.kw_equal(ident, "def")) break :blk .{.s = ident.s, .e = ident.e, .tag = .def};
                    if (self.kw_equal(ident, "extern")) break :blk .{.s = ident.s, .e = ident.e, .tag = .tag_extern};
                    break :blk ident;
                }
                if (isnum(self.char)) {
                    break :blk self.read_number();
                }

                self.nextChar();
                break :blk .{.s = self.i - 1, .e = self.i, .tag = .unknown};
            }
        };

        return token;
    }

    fn skipWhitechars(self: *Self) void {
        while (isspace(self.char)) {
            self.nextChar();
        }
    }

    fn charToken(self: *Self, tag: Tag) Token {
        const token: Token = .{.s = self.i, .e = self.i+1, .tag = tag};
        self.nextChar();
        return token;
    }

    fn nextChar(self: *Self) void {
        self.i += 1;
        if (self.i >= self.buf.len) {
            self.char = 0;
        } else {
            self.char = self.buf[self.i];
        }
    }

    fn read_identifier(self: *Self) Token {
        std.debug.assert(isalpha(self.char));
        const pos = self.i;
        while (isalphanum(self.char)) {
            self.nextChar();
        }

        return .{.s = pos, .e = self.i, .tag = .identifier};
    }

    fn skipComment(self: *Self) void {
        while (!(isnewline(self.char)) and self.char != 0) {
            self.nextChar();
        }
    }

    fn read_number(self: *Self) Token {
        std.debug.assert(isnum(self.char));
        const pos = self.i;
        while (isnum(self.char)) {
            self.nextChar();
        }

        return .{.s = pos, .e = self.i, .tag = .number};
    }

    fn kw_equal(self: Self, token: Token, kw: []const u8) bool {
        return std.mem.eql(u8, self.buf[token.s..token.e], kw);
    }

    pub fn inspect(self: Self, token: Token) []const u8 {
        return self.buf[token.s..token.e];
    }
};


test "identifier" {
    const buffer = "fib";
    var lexer = Lexer.create(buffer);

    const token = lexer.nextToken();
    try testing.expect(token.tag == .identifier);
    try testing.expectEqualStrings(lexer.inspect(token), "fib");

    const token_eof = lexer.nextToken();
    try testing.expect(token_eof.tag == .eof);
}

test "def" {
    const expected = [_] struct {[]const u8, Tag} {
        .{"def", .def},
        .{"fib", .identifier},
        .{"(", .lparen},
        .{"x", .identifier},
        .{")", .rparen},
        .{",", .comma},
        .{"<", .lt},
        .{">", .gt},
        .{"+", .plus},
        .{"-", .minus},
        .{"*", .multiply},
        .{"/", .divide},
    };
    const buffer = "def fib(x),<>+-*/";
    var lexer = Lexer.create(buffer);

    for (expected) |p| {
        const token = lexer.nextToken();
        // std.debug.print("{}:{}\n", .{token.tag, p.@"1"});
        // std.debug.print("{s}:{s}\n", .{lexer.inspect(token), p.@"0"});
        try testing.expect(token.tag == p.@"1");
        try testing.expectEqualStrings(lexer.inspect(token), p.@"0");
    }
    const token_eof = lexer.nextToken();
    try testing.expect(token_eof.tag == .eof);
}

test "numbers" {
    const expected = [_] struct {[]const u8, Tag} {
        .{"35", .number},
        .{"foo2", .identifier},
        .{"29.48", .number},
    };
    const buffer = "35 foo2   29.48";
    var lexer = Lexer.create(buffer);

    for (expected) |p| {
        const token = lexer.nextToken();
        try testing.expect(token.tag == p.@"1");
        try testing.expectEqualStrings(lexer.inspect(token), p.@"0");
    }
    const token_eof = lexer.nextToken();
    try testing.expect(token_eof.tag == .eof);
}

test "comments" {
    const expected: [2] struct {[]const u8, Tag} = .{
        .{"foo", .identifier},
        .{"bar", .identifier},
    };
    const buffer = "foo # my comment\n# another comment\n\nbar";
    var lexer = Lexer.create(buffer);

    for (expected) |p| {
        const token = lexer.nextToken();
        try testing.expect(token.tag == p.@"1");
        try testing.expectEqualStrings(lexer.inspect(token), p.@"0");
    }
    const token_eof = lexer.nextToken();
    try testing.expect(token_eof.tag == .eof);
}
