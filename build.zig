const std = @import("std");

const name = "kaleidoscope";

fn makeModule(b: *std.Build) !*std.Build.Module {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const module = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });

    const llvm_config = try b.findProgram(&[_][]const u8{ "llvm-config" }, &[_][]const u8{});

    const include_path_raw = b.run(&[_][]const u8{ llvm_config, "--includedir" });
    const include_path = std.mem.trimRight(u8, include_path_raw, "\t\r\n");
    module.addIncludePath(.{ .cwd_relative = include_path });

    const lib_path_raw = b.run(&[_][]const u8{ llvm_config, "--libdir" });
    const lib_path = std.mem.trimRight(u8, lib_path_raw, "\t\r\n");
    module.addLibraryPath(.{ .cwd_relative = lib_path });

    const libs = b.run(&[_][]const u8{ llvm_config, "--libs" });
    var it = std.mem.splitScalar(u8, libs, ' ');
    while (it.next()) |lib_raw| {
        var lib = std.mem.trim(u8, lib_raw, " \t\r\n");
        if (lib.len <= 2) {
            continue;
        }
        lib = lib[2..];
        module.linkSystemLibrary(lib, .{});
    }

    return module;
}

pub fn build(b: *std.Build) !void {
    const module = try makeModule(b);

    const exe = b.addExecutable(.{
        .name = name,
        .root_module = module,
    });

    b.installArtifact(exe);
    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());

    // This allows the user to pass arguments to the application in the build
    // command itself, like this: `zig build run -- arg1 arg2 etc`
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    // This creates a build step. It will be visible in the `zig build --help` menu,
    // and can be selected like this: `zig build run`
    // This will evaluate the `run` step rather than the default, which is "install".
    const run_step = b.step("run", "Run the kaleidoscope");
    run_step.dependOn(&run_cmd.step);

    // === Additional targets ===

    // Build without run
    const build_step = b.step("build", "Build kaleidoscope.zig only");
    build_step.dependOn(&exe.step);

    // === Tests ===

    // Tests for main
    const unit_tests = b.addTest(.{
        .root_module = module,
    });

    const run_unit_tests = b.addRunArtifact(unit_tests);
    const test_step = b.step("test", "Run unit tests for main.zig");
    test_step.dependOn(&run_unit_tests.step);
}
