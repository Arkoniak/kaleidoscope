const std = @import("std");
const testing = std.testing;
const lexer = @import("lexer.zig");

pub const Expr = union(enum) {
    Function: FunctionExpr,
    Number: NumberExpr,
    Variable: VariableExpr,
    Call: CallExpr,
    BinOp: BinOpExpr,
    Prototype: PrototypeExpr,

    const Self = @This();
    pub fn number(allocator: std.mem.Allocator, value: f64) !*Self {
        const expr = try allocator.create(Expr);
        expr.* = Expr{.Number = NumberExpr{.value = value}};
        return expr;
    }

    pub fn function(allocator: std.mem.Allocator, proto: *const Expr, body: *const Expr) !*Self {
        const expr = try allocator.create(Expr);
        expr.* = Expr{.Function = FunctionExpr{.proto = proto, .body = body}};
        return expr;
    }

    pub fn variable(allocator: std.mem.Allocator, ident: []const u8) !*Self {
        const expr = try allocator.create(Expr);
        expr.* = Expr{.Variable = VariableExpr{.ident = ident}};
        return expr;
    }

    pub fn call(allocator: std.mem.Allocator, name: []const u8, args: []*Expr) !*Self {
        const expr = try allocator.create(Expr);
        expr.* = Expr{.Call = CallExpr{.name = name, .args = args}};
        return expr;
    }

    pub fn binop(allocator: std.mem.Allocator, op: lexer.Tag, lhs: *Expr, rhs: *Expr) !*Self {
        const expr = try allocator.create(Expr);
        expr.* = Expr{.BinOp = BinOpExpr{.op = op, .lhs = lhs, .rhs = rhs}};
        return expr;
    }

    pub fn prototype(allocator: std.mem.Allocator, name: []const u8, args: []const []const u8)!*Self {
        const expr = try allocator.create(Expr);
        expr.* = Expr{.Prototype = PrototypeExpr{.name = name, .args = args}};
        return expr;
    }

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch(self) {
            .Function  => |func_expr| try func_expr.format(fmt, options, writer),
            .Number    => |number_expr| try number_expr.format(fmt, options, writer),
            .Variable  => |var_expr| try var_expr.format(fmt, options, writer),
            .Call      => |call_expr| try call_expr.format(fmt, options, writer),
            .BinOp     => |binop_expr| try binop_expr.format(fmt, options, writer),
            .Prototype => |proto_expr| try proto_expr.format(fmt, options, writer),
        }
    }
};

pub const NumberExpr = struct {
    value: f64,

    const Self = @This();
    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("{d}", .{self.value});
    }
};

pub const VariableExpr = struct {
    ident: []const u8,

    const Self = @This();
    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("{s}", .{self.ident});
    }
};

const BinOpExpr = struct {
    op: lexer.Tag,
    lhs: *Expr,
    rhs: *Expr,

    const Self = @This();
    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {   
        _ = fmt;
        _ = options;

        const op: u8 = switch(self.op) {
            .lt => '<',
            .gt => '>',
            .minus => '-',
            .plus => '+',
            .multiply => '*',
            .divide => '/',
            else => '?',
        };
        try writer.print("({} {c} {})", .{self.lhs, op, self.rhs});
    }
};

const FunctionExpr = struct {
    proto: *const Expr,
    body: *const Expr,

    const Self = @This();
    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("{} {{\n    ret ", .{self.proto});
        try writer.print("{}", .{self.body});
        try writer.print("\n}}", .{});
    }
};

const CallExpr = struct {
    name: []const u8,
    args: []*Expr,

    const Self = @This();
    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("{s}(", .{self.name});
        var isFirst: bool = true;
        for (self.args) |arg| {
            if (isFirst) {
                isFirst = false;
            } else {
                try writer.print(", ", .{});
            }
            try writer.print("{}", .{arg});
        }
        try writer.print(")", .{});
    }
};

const PrototypeExpr = struct {
    name: []const u8,
    args: []const []const u8,

    const Self = @This();
    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("{s}(", .{self.name});
        var isFirst: bool = true;
        for (self.args) |arg| {
            if (isFirst) {
                isFirst = false;
            } else {
                try writer.print(" ", .{});
            }

            try writer.print("{s}", .{arg});
        }
        try writer.print(")", .{});
    }
};

test "Simple AST creation" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const num = try Expr.number(allocator, 5);
    try testing.expectEqual(num.Number.value, 5);

    const args = std.ArrayList([]const u8).init(allocator);
    const proto = try Expr.prototype(allocator, "_lambda", args.items);
    const func = try Expr.function(allocator, proto, num);
    try testing.expectEqual(func.Function.body.Number.value, 5);

    const variable = try Expr.variable(allocator, "foo");
    try testing.expectEqualStrings(variable.Variable.ident, "foo");
}

test "Simple print number usage" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    var buffer: [4096]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buffer);
    const writer = stream.writer();

    const num = try Expr.number(allocator, 5);
    try writer.print("{}", .{num});
    try testing.expectEqualStrings(stream.getWritten(), "5");
}

test "Simple print function usage" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    var buffer: [4096]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buffer);
    const writer = stream.writer();

    const proto = try Expr.prototype(allocator, "_lambda", &[_][]const u8{});
    const num = try Expr.number(allocator, 5);
    const func = try Expr.function(allocator, proto, num);
    try writer.print("{}", .{func});
    try testing.expectEqualStrings(stream.getWritten(), "_lambda() {\n    ret 5\n}");
}

test "Prorotyped function with multiple variables" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    var buffer: [4096]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buffer);
    const writer = stream.writer();

    const proto = try Expr.prototype(allocator, "_lambda", &[_][]const u8{"x", "y"});

    const lhs = try Expr.variable(allocator, "x");
    const rhs = try Expr.number(allocator, 5);
    const op = lexer.Tag.plus;

    const binop = try Expr.binop(allocator, op, lhs, rhs);
    const func = try Expr.function(allocator, proto, binop);
    try writer.print("{}", .{func});
    try testing.expectEqualStrings(stream.getWritten(), "_lambda(x y) {\n    ret (x + 5)\n}");
}

test "Simple function call usage" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    var buffer: [4096]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buffer);
    const writer = stream.writer();

    const var1 = try Expr.variable(allocator, "x");
    const var2 = try Expr.variable(allocator, "y");
    var args_buf = [_]*Expr{var1, var2};
    const args: []*Expr = args_buf[0..];
    const fname = "foo";
    const call = try Expr.call(allocator, fname, args);

    try writer.print("{}", .{call});
    try testing.expectEqualStrings(stream.getWritten(), "foo(x, y)");
}

test "Simple binops usage" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    var buffer: [4096]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buffer);
    const writer = stream.writer();

    const lhs = try Expr.variable(allocator, "x");
    const rhs = try Expr.number(allocator, 5);
    const op = lexer.Tag.plus;

    const binop = try Expr.binop(allocator, op, lhs, rhs);
    try writer.print("{}", .{binop});
    try testing.expectEqualStrings(stream.getWritten(), "(x + 5)");
}

test "Nested binops usage" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    var buffer: [4096]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buffer);
    const writer = stream.writer();

    const lhs = try Expr.variable(allocator, "x");
    const rhs = try Expr.number(allocator, 5);
    const op = lexer.Tag.plus;
    const binop1 = try Expr.binop(allocator, op, lhs, rhs);

    const rhs2 = try Expr.variable(allocator, "y");
    const op2 = lexer.Tag.multiply;
    const binop = try Expr.binop(allocator, op2, binop1, rhs2);
    try writer.print("{}", .{binop});
    try testing.expectEqualStrings(stream.getWritten(), "((x + 5) * y)");
}
