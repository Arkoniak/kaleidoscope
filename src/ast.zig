const std = @import("std");
const testing = std.testing;

pub const Expr = union(enum) {
    Function: FunctionExpr,
    Number: NumberExpr,

    const Self = @This();
    pub fn number(allocator: std.mem.Allocator, value: f64) !*Self {
        const expr = try allocator.create(Expr);
        expr.* = Expr{.Number = NumberExpr{.value = value}};
        // expr.Number = NumberExpr{.value = value};
        return expr;
    }

    pub fn function(allocator: std.mem.Allocator, body: *const Expr) !*Self {
        const expr = try allocator.create(Expr);
        expr.* = Expr{.Function = FunctionExpr{.body = body}};
        // expr.Function = FunctionExpr{.body = body};
        return expr;
    }

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch(self) {
            .Function => |func_expr| try func_expr.format(fmt, options, writer),
            .Number => |number_expr| try number_expr.format(fmt, options, writer),
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

const FunctionExpr = struct {
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

        try writer.print("double _lambda() {{\n    ret ", .{});
        try writer.print("{}", .{self.body});
        try writer.print("\n}}", .{});
    }
};

test "Simple AST creation" {
    const allocator = std.testing.allocator;
    const num = try Expr.number(allocator, 5);
    defer allocator.destroy(num);
    try testing.expectEqual(num.Number.value, 5);

    const func = try Expr.function(allocator, num);
    defer allocator.destroy(func);
    try testing.expectEqual(func.Function.body.Number.value, 5);
}

test "Simple print number usage" {
    const allocator = std.testing.allocator;
    var buffer: [4096]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buffer);
    const writer = stream.writer();

    const num = try Expr.number(allocator, 5);
    defer allocator.destroy(num);
    try writer.print("{}", .{num});
    try testing.expectEqualStrings(stream.getWritten(), "5");
}

test "Simple print function usage" {
    const allocator = std.testing.allocator;
    var buffer: [4096]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buffer);
    const writer = stream.writer();

    const num = try Expr.number(allocator, 5);
    defer allocator.destroy(num);
    const func = try Expr.function(allocator, num);
    defer allocator.destroy(func);
    try writer.print("{}", .{func});
    try testing.expectEqualStrings(stream.getWritten(), "double _lambda() {\n    ret 5\n}");
}
