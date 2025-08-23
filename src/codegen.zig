const std = @import("std");
const testing = std.testing;
const String = []const u8;

const llvm = @cImport({
    @cInclude("llvm-c/Core.h");
    @cInclude("llvm-c/Target.h");
    @cInclude("llvm-c/Types.h");
});

const Context = llvm.LLVMContextRef;
const Builder = llvm.LLVMBuilderRef;
const Module = llvm.LLVMModuleRef;
const Value = llvm.LLVMValueRef;
const LLVMType = llvm.LLVMTypeRef;

pub const VM = struct {
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

test "Dummy" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var vm = try VM.init("kaleidoscope", allocator);
    defer vm.deinit(allocator);
    try testing.expectEqual(1, 1);
}
