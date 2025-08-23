const std = @import("std");
const testing = std.testing;
const String = []const u8;

pub const llvm = @cImport({
    @cInclude("llvm-c/Core.h");
    @cInclude("llvm-c/Target.h");
    @cInclude("llvm-c/Types.h");
});

const Context = llvm.LLVMContextRef;
const Builder = llvm.LLVMBuilderRef;
const Module = llvm.LLVMModuleRef;
const Value = llvm.LLVMValueRef;
const LLVMType = llvm.LLVMTypeRef;

const KaleidoscopeError = error{ UnknownVariableName, UnknownFunctionName };

const ExprType = enum { PrototypeExpr, FunctionExpr, BinaryExpr, NumberExpr, VariableExpr, CallExpr };

const Expr = struct {
    etype: ExprType,
    expr: union(ExprType) { PrototypeExpr: ProtoAST, FunctionExpr: FunctionAST, BinaryExpr: BinaryAST, NumberExpr: NumberAST, VariableExpr: VariableAST, CallExpr: CallAST },

    const Self = @This();

    pub fn call(name: String, args: []Expr) Self {
        const callast: CallAST = .{ .name = name, .args = args };
        return .{ .etype = .CallExpr, .expr = .{ .CallExpr = callast } };
    }

    pub fn func(name: String, args: []String, body: *Expr) Self {
        const funcast: FunctionAST = .{ .name = name, .args = args, .body = body };
        return .{ .etype = .FunctionExpr, .expr = .{ .FunctionExpr = funcast } };
    }

    pub fn number(val: f64) Self {
        const numerast: NumberAST = .{ .val = val };
        return .{ .etype = .NumberExpr, .expr = .{ .NumberExpr = numerast } };
    }

    pub fn proto(name: String, args: []String) Self {
        const protoast: ProtoAST = .{ .name = name, .args = args };
        return .{ .etype = .PrototypeExpr, .expr = .{ .PrototypeExpr = protoast } };
    }

    // It's better to use `const MyError = error{SomeError, AnotherError};` and provide
    // it as an error set

    pub fn codegen(self: *Self, vm: VM, allocator: std.mem.Allocator) anyerror!Value {
        return switch (self.etype) {
            .PrototypeExpr => try self.expr.PrototypeExpr.codegen(vm, allocator),
            .FunctionExpr => try self.expr.FunctionExpr.codegen(vm, allocator),
            .BinaryExpr => try self.expr.BinaryExpr.codegen(vm, allocator),
            .NumberExpr => try self.expr.NumberExpr.codegen(vm, allocator),
            .VariableExpr => try self.expr.VariableExpr.codegen(vm, allocator),
            .CallExpr => try self.expr.CallExpr.codegen(vm, allocator),
        };
    }
};

const NumberAST = struct {
    val: f64,

    const Self = @This();

    pub fn codegen(self: *const Self, vm: VM, allocator: std.mem.Allocator) !Value {
        _ = allocator;

        const tf64: LLVMType = llvm.LLVMDoubleTypeInContext(vm.context);
        const n1: Value = llvm.LLVMConstReal(tf64, self.val);

        return n1;
    }
};

const VariableAST = struct {
    name: String,

    const Self = @This();

    pub fn codegen(self: *const Self, vm: VM, allocator: std.mem.Allocator) !Value {
        _ = allocator;
        const val = vm.namedValues.get(self.name) orelse {
            std.log.err("Unknown variable name {s}\n", .{self.name});
            return KaleidoscopeError.UnknownVariableName;
        };

        return val;
    }
};

const BinaryAST = struct {
    lhs: *Expr,
    rhs: *Expr,

    const Self = @This();

    pub fn codegen(self: *const Self, vm: VM, allocator: std.mem.Allocator) !Value {
        // const tf64: llvm.LLVMTypeRef = llvm.LLVMDoubleTypeInContext(vm.context);
        const n1: Value = try self.lhs.codegen(vm, allocator);
        const n2: Value = try self.rhs.codegen(vm, allocator);

        const addtmp = llvm.LLVMBuildFAdd(vm.builder, n1, n2, "addtmp");

        return addtmp;
    }
};

const CallAST = struct {
    name: String,
    args: []Expr,

    const Self = @This();

    pub fn codegen(self: *Self, vm: VM, allocator: std.mem.Allocator) !Value {
        const cname: [*c]const u8 = @ptrCast(self.name.ptr);

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
            const val: Value = try arg.codegen(vm, allocator);
            try argsv.append(val);
        }

        // Getting function type
        const function_type = llvm.LLVMGlobalGetValueType(callee);
        return llvm.LLVMBuildCall2(vm.builder, function_type, callee, argsv.items.ptr, @intCast(argsv.items.len), "calltmp");
    }
};

const ProtoAST = struct {
    name: String,
    args: []String,

    const Self = @This();

    pub fn codegen(self: *Self, vm: VM, allocator: std.mem.Allocator) !Value {
        const tf64: LLVMType = llvm.LLVMDoubleTypeInContext(vm.context);

        // allocate vector of llvm.LLVMTypeRef; params[i] = tf64;
        var param_types = std.ArrayList(LLVMType).init(allocator);
        for (0..self.args.len) |i| {
            _ = i;
            try param_types.append(tf64);
        }
        defer param_types.deinit();

        const len: c_uint = @intCast(param_types.items.len);

        const func_type: LLVMType = llvm.LLVMFunctionType(tf64, // return type
            param_types.items.ptr, // param types
            len, // number of params
            0 // non variadic (false)
        );

        // Create function with external linkage
        const cname: [*c]const u8 = @ptrCast(self.name.ptr);
        const func: Value = llvm.LLVMAddFunction(vm.module, cname, func_type);
        llvm.LLVMSetLinkage(func, llvm.LLVMExternalLinkage);

        for (0..len) |i| {
            const j: c_uint = @intCast(i);
            const param: Value = llvm.LLVMGetParam(func, j);

            const arg_name: [*c]const u8 = @ptrCast(self.args[i].ptr);
            const arg_len: c_uint = @intCast(self.args[i].len);
            llvm.LLVMSetValueName2(param, arg_name, arg_len);
        }

        return func;
    }
};

const FunctionAST = struct {
    name: String,
    args: []String,

    body: *Expr,

    const Self = @This();

    pub fn codegen(self: *Self, vm: VM, allocator: std.mem.Allocator) !Value {
        const cname: [*c]const u8 = @ptrCast(self.name.ptr);

        const pre_func: Value = llvm.LLVMGetNamedFunction(vm.module, cname);

        const func = if (pre_func == null) blk: {
            var proto_ast = ProtoAST{ .name = self.name, .args = self.args };
            break :blk try proto_ast.codegen(vm, allocator);
        } else pre_func;

        vm.update_names(func, allocator);

        // Create a new basic block to start insertion into.
        const entry: llvm.LLVMBasicBlockRef = llvm.LLVMAppendBasicBlockInContext(vm.context, func, "entry");
        llvm.LLVMPositionBuilderAtEnd(vm.builder, entry);

        const b = try self.body.codegen(vm, allocator);
        _ = llvm.LLVMBuildRet(vm.builder, b);

        return func;
    }
};

pub fn LLVM() type {
    return struct {
        context: Context,
        builder: Builder,
        module: Module,
        namedValues: *Map,

        const Self = @This();
        const Map = std.StringHashMap(Value);

        pub fn init(module_name: String, allocator: std.mem.Allocator) !Self {
            const context: Context = llvm.LLVMContextCreate();
            const cname: [*c]const u8 = @ptrCast(module_name.ptr);
            const module: Module = llvm.LLVMModuleCreateWithNameInContext(cname, context);
            const builder: Builder = llvm.LLVMCreateBuilderInContext(context);

            const namedValues = try allocator.create(Map);
            namedValues.* = Map.init(allocator);

            return .{ .context = context, .builder = builder, .module = module, .namedValues = namedValues };
        }

        pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
            self.namedValues.deinit();
            allocator.destroy(self.namedValues);
            llvm.LLVMDisposeBuilder(self.builder);
            llvm.LLVMDisposeModule(self.module);
            llvm.LLVMContextDispose(self.context);
            llvm.LLVMShutdown();
        }

        pub fn update_names(self: *Self, func: Value, allocator: std.mem.Allocator) !void {
            self.namedValues.clearRetainingCapacity();

            const param_count = llvm.LLVMCountParams(func);
            if (param_count == 0) return;

            const params = try allocator.alloc(Value, param_count);
            defer allocator.free(params);

            llvm.LLVMGetParams(func, params.ptr);
            for (params) |param| {
                const param_name_ptr = llvm.LLVMGetValueName(param);
                const param_name = if (param_name_ptr != null)
                    std.mem.span(param_name_ptr)
                else
                    "<unnamed>";
                try self.namedValues.put(param_name, param);
            }
        }
    };
}

const VM = LLVM();

test "Dummy" {
    try testing.expect(1 == 1);
}
