const std = @import("std");
const testing = std.testing;
const lexer = @import("lexer.zig");
const vm_module = @import("vm.zig");
const VM = vm_module.VM;
const Value = vm_module.Value;
const LLVMType = llvm.LLVMTypeRef;
const llvm = vm_module.llvm;

const ExprError = error{ UnknownVariableName, UnknownFunctionName, UnknownArithmeticOperation, NoPrototypeInFunctionDeclaration};
const KaleidoscopeError = ExprError || std.mem.Allocator.Error;


pub fn toCStr(allocator: std.mem.Allocator, input: []const u8) ![*c]const u8 {
    const len = input.len;
    const buf = try allocator.alloc(u8, len + 1);
    @memcpy(buf[0..len], input);
    buf[len] = 0;
    const result: [*c]const u8 = @ptrCast(buf.ptr);

    return result;
}


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

    pub fn call(allocator: std.mem.Allocator, name: []const u8, args: []const *Expr) !*Self {
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

    pub fn codegen(self: Self, vm: VM, allocator: std.mem.Allocator) KaleidoscopeError!Value {
        return switch(self) {
            .Function => |func_expr| try func_expr.codegen(vm, allocator),
            .Number => |number_expr| number_expr.codegen(vm, allocator),
            .Variable => |var_expr| try var_expr.codegen(vm, allocator),
            .Call => |call_expr| try call_expr.codegen(vm, allocator),
            .BinOp => |binop_expr| try binop_expr.codegen(vm, allocator),
            .Prototype => |proto_expr| try proto_expr.codegen(vm, allocator),
        };
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
    pub fn codegen(self: Self, vm: VM, allocator: std.mem.Allocator) Value {
        _ = allocator;
        return vm.ConstReal(self.value);
    }

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

    pub fn codegen(self: Self, vm: VM, allocator: std.mem.Allocator) KaleidoscopeError!Value {
        _ = allocator;

        const val = vm.namedValues.get(self.ident) orelse {
            std.log.err("Unknown variable name {s}\n", .{self.ident});
            return KaleidoscopeError.UnknownVariableName;
        };

        return val;
    }

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

    pub fn codegen(self: Self, vm: VM, allocator: std.mem.Allocator) KaleidoscopeError!Value {
        const n1: Value = try self.lhs.codegen(vm, allocator);
        const n2: Value = try self.rhs.codegen(vm, allocator);

        return switch (self.op) {
            .plus => llvm.LLVMBuildFAdd(vm.builder, n1, n2, "addtmp"),
            .minus => llvm.LLVMBuildFSub(vm.builder, n1, n2, "subtmp"),
            .multiply => llvm.LLVMBuildFMul(vm.builder, n1, n2, "multmp"),
            .divide => llvm.LLVMBuildFDiv(vm.builder, n1, n2, "divtmp"),
            .lt => self.makeCmp(vm, n1, n2, "ltbooltmp"),
            .gt => self.makeCmp(vm, n2, n1, "gtbooltmp"),
            else => KaleidoscopeError.UnknownArithmeticOperation,
        };
    }

    fn makeCmp(self: Self, vm: VM, lhs: Value, rhs: Value, name: [*c]const u8) Value {
        _ = self;
        const val = llvm.LLVMBuildFCmp(vm.builder, llvm.LLVMRealULT, lhs, rhs, "cmptmp");

        return llvm.LLVMBuildUIToFP(vm.builder, val, vm.tf64, name);
    }

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

    pub fn codegen(self: Self, vm: VM, allocator: std.mem.Allocator) KaleidoscopeError!Value {
        const proto = switch(self.proto.*) {
            .Prototype => |proto_expr| proto_expr,
            else => return KaleidoscopeError.NoPrototypeInFunctionDeclaration,
        };
        const cname = try toCStr(allocator, proto.name);

        const pre_func: Value = llvm.LLVMGetNamedFunction(vm.module, cname);

        const func = if (pre_func == null) blk: {
            break :blk try self.proto.codegen(vm, allocator);
        } else pre_func;

        try vm.update_names(func, allocator);

        // Create a new basic block to start insertion into.
        const entry: llvm.LLVMBasicBlockRef = llvm.LLVMAppendBasicBlockInContext(vm.context, func, "entry");
        llvm.LLVMPositionBuilderAtEnd(vm.builder, entry);

        const b = try self.body.codegen(vm, allocator);
        _ = llvm.LLVMBuildRet(vm.builder, b);

        return func;
    }

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
    args: []const *Expr,

    const Self = @This();

    pub fn codegen(self: Self, vm: VM, allocator: std.mem.Allocator) KaleidoscopeError!Value {
        const cname = try toCStr(allocator, self.name);

        const callee: Value = llvm.LLVMGetNamedFunction(vm.module, cname);
        if (callee == null) {
            std.log.err("Unknown function reference: {s}\n", .{self.name});
            return KaleidoscopeError.UnknownFunctionName;
        }
        const param_count = llvm.LLVMCountParams(callee);
        if (param_count != self.args.len) {
            std.log.err("Incorrect # arguments passed: function: {d}, passed: {d}\n", .{ param_count, self.args.len });
            return KaleidoscopeError.UnknownFunctionName;
        }

        var argsv = std.ArrayList(Value).init(allocator);
        defer argsv.deinit();

        for (self.args) |*arg| {
            const val: Value = try arg.*.codegen(vm, allocator);
            try argsv.append(val);
        }

        // Getting function type
        const function_type = llvm.LLVMGlobalGetValueType(callee);
        return llvm.LLVMBuildCall2(vm.builder, function_type, callee, argsv.items.ptr, @intCast(argsv.items.len), "calltmp");
    }

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

    pub fn codegen(self: Self, vm: VM, allocator: std.mem.Allocator) KaleidoscopeError!Value {
        // allocate vector of llvm.LLVMTypeRef; params[i] = tf64;
        var param_types = std.ArrayList(LLVMType).init(allocator);
        for (0..self.args.len) |i| {
            _ = i;
            try param_types.append(vm.tf64);
        }
        defer param_types.deinit();

        const len: c_uint = @intCast(param_types.items.len);

        const func_type: LLVMType = llvm.LLVMFunctionType(vm.tf64, // return type
            param_types.items.ptr, // param types
            len, // number of params
            0 // non variadic (false)
        );

        // Create function with external linkage
        const cname = try toCStr(allocator, self.name);
        const func: Value = llvm.LLVMAddFunction(vm.module, cname, func_type);
        llvm.LLVMSetLinkage(func, llvm.LLVMExternalLinkage);

        for (0..len) |i| {
            const j: c_uint = @intCast(i);
            const param: Value = llvm.LLVMGetParam(func, j);

            // const arg_name: [*c]const u8 = @ptrCast(self.args[i].ptr);
            const arg_name = try toCStr(allocator, self.args[i]);
            const arg_len: c_uint = @intCast(self.args[i].len);
            llvm.LLVMSetValueName2(param, arg_name, arg_len);
        }

        return func;
    }

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
    const fname = "foo";
    const call = try Expr.call(allocator, fname, &[_]*Expr{var1, var2});

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

test "Codegen number expression" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const vm = try VM.init("kaleidoscope", allocator);

    const num = try Expr.number(allocator, 5);
    const value = try num.codegen(vm, allocator) orelse unreachable;
    const value_str = try vm_module.valueToString(value, allocator);
    try testing.expectEqualStrings("double 5.000000e+00", value_str);
}

test "Codegen variable expression" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const vm = try VM.init("kaleidoscope", allocator);

    const variable = try Expr.variable(allocator, "foo");
    const num = try Expr.number(allocator, 5);
    const num_value = try num.codegen(vm, allocator) orelse unreachable;
    try vm.put("foo", num_value);
    const value = try variable.codegen(vm, allocator) orelse unreachable;
    const value_str = try vm_module.valueToString(value, allocator);
    try testing.expectEqualStrings("double 5.000000e+00", value_str);
}

test "Codegen binop expression" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const vm = try VM.init("kaleidoscope", allocator);

    const lhs = try Expr.number(allocator, 5);
    const rhs = try Expr.number(allocator, 2);
    const op = lexer.Tag.plus;
    var binop = try Expr.binop(allocator, op, lhs, rhs);

    var value = try binop.codegen(vm, allocator);
    var value_str = try vm_module.valueToString(value, allocator);
    try testing.expectEqualStrings("double 7.000000e+00", value_str);

    binop = try Expr.binop(allocator, lexer.Tag.minus, lhs, rhs);
    value = try binop.codegen(vm, allocator);
    value_str = try vm_module.valueToString(value, allocator);
    try testing.expectEqualStrings("double 3.000000e+00", value_str);

    binop = try Expr.binop(allocator, lexer.Tag.multiply, lhs, rhs);
    value = try binop.codegen(vm, allocator);
    value_str = try vm_module.valueToString(value, allocator);
    try testing.expectEqualStrings("double 1.000000e+01", value_str);

    binop = try Expr.binop(allocator, lexer.Tag.divide, lhs, rhs);
    value = try binop.codegen(vm, allocator);
    value_str = try vm_module.valueToString(value, allocator);
    try testing.expectEqualStrings("double 2.500000e+00", value_str);

    binop = try Expr.binop(allocator, lexer.Tag.lt, lhs, rhs);
    value = try binop.codegen(vm, allocator);
    value_str = try vm_module.valueToString(value, allocator);
    try testing.expectEqualStrings("double 0.000000e+00", value_str);

    binop = try Expr.binop(allocator, lexer.Tag.gt, lhs, rhs);
    value = try binop.codegen(vm, allocator);
    value_str = try vm_module.valueToString(value, allocator);
    try testing.expectEqualStrings("double 1.000000e+00", value_str);
}

test "Codegen for prototype" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const vm = try VM.init("kaleidoscope", allocator);

    const proto = try Expr.prototype(allocator, "_lambda", &[_][]const u8{"x", "y"});

    const value = try proto.codegen(vm, allocator);
    const value_str = try vm_module.valueToString(value, allocator);
    try testing.expectEqualStrings("declare double @_lambda(double, double)\n", value_str);
}

test "Codegen function with multiple variables" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const vm = try VM.init("kaleidoscope", allocator);

    const proto = try Expr.prototype(allocator, "_lambda", &[_][]const u8{"x", "y"});

    const lhs = try Expr.variable(allocator, "x");
    const rhs = try Expr.number(allocator, 5);
    const op = lexer.Tag.plus;

    const binop = try Expr.binop(allocator, op, lhs, rhs);
    const func = try Expr.function(allocator, proto, binop);

    const value = try func.codegen(vm, allocator);
    const value_str = try vm_module.valueToString(value, allocator);
    try testing.expectEqualStrings("define double @_lambda(double %x, double %y) {\nentry:\n  %addtmp = fadd double %x, 5.000000e+00\n  ret double %addtmp\n}\n", value_str);
}

test "Codegen call expression" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const vm = try VM.init("kaleidoscope", allocator);

    const proto = try Expr.prototype(allocator, "foo", &[_][]const u8{"x", "y"});

    const lhs = try Expr.variable(allocator, "x");
    const rhs = try Expr.number(allocator, 5);
    const op = lexer.Tag.plus;

    const binop = try Expr.binop(allocator, op, lhs, rhs);
    const func = try Expr.function(allocator, proto, binop);
    _ = try func.codegen(vm, allocator);

    const var1 = try Expr.variable(allocator, "x");
    const var2 = try Expr.variable(allocator, "y");
    const fname = "foo";
    const call = try Expr.call(allocator, fname, &[_]*Expr{var1, var2});

    const value = try call.codegen(vm, allocator);
    const value_str = try vm_module.valueToString(value, allocator);
    try testing.expectEqualStrings("  %calltmp = call double @foo(double %x, double %y)", value_str);
}

