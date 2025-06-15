const std = @import("std");
const testing = std.testing;
const String = []const u8;

// const ExprTag = enum {
//     number,
//     variable,
//     binary,
// };

const Expr = union(enum) {
    number: NumberExpr,
    variable: VariableExpr,
    binary: BinaryExpr,
};

const NumberExpr = struct {
    val: f64,
};

const VariableExpr = struct {
    name: String,
};

const BinaryExpr = struct {
    op: u8,

    lhs: *const Expr,
    rhs: *const Expr,
};

const FunctionAST = struct {
    name: String,
    args: []const String,
    body: *const Expr,
};

test "Dummy" {
    try testing.expect(1 == 1);
}
