const std = @import("std");

const part1 = "src/05-1.zig";
const part2 = "src/05-2.zig";
const name1 = "aoc-5-1";
const name2 = "aoc-5-2";

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const exe_mod = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    // Modules can depend on one another using the `std.Build.Module.addImport` function.
    // This is what allows Zig source code to use `@import("foo")` where 'foo' is not a
    // file path. In this case, we set up `exe_mod` to import `lib_mod`.
    // exe_mod.addImport("_01_lib", lib_mod);

    // Now, we will create a static library based on the module we created above.
    // This creates a `std.Build.Step.Compile`, which is the build step responsible
    // for actually invoking the compiler.
    // const lib = b.addLibrary(.{
    //     .linkage = .static,
    //     .name = "_01",
    //     .root_module = lib_mod,
    // });

    // This declares intent for the library to be installed into the standard
    // location when the user invokes the "install" step (the default step when
    // running `zig build`).
    // b.installArtifact(lib);

    const exe = b.addExecutable(.{
        .name = "kaleidoscope",
        .root_module = exe_mod,
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

    // === ДОПОЛНИТЕЛЬНЫЕ ЦЕЛИ ===

    // Цель для сборки без запуска
    const build_step = b.step("build", "Build kaleidoscope.zig only");
    build_step.dependOn(&exe.step);

    // === ТЕСТЫ (если нужны) ===

    // Тесты для main
    const unit_tests = b.addTest(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const run_unit_tests = b.addRunArtifact(unit_tests);
    const test_step = b.step("test", "Run unit tests for main.zig");
    test_step.dependOn(&run_unit_tests.step);

    // // Общий step для всех тестов
    // const test_step = b.step("test", "Run all unit tests");
    // test_step.dependOn(&run_unit_tests1.step);
    // test_step.dependOn(&run_unit_tests2.step);

    //     // Creates a step for unit testing. This only builds the test executable
    //     // but does not run it.
    //     // const lib_unit_tests = b.addTest(.{
    //     //     .root_module = lib_mod,
    //     // });

    //     // const run_lib_unit_tests = b.addRunArtifact(lib_unit_tests);

    //     const exe_unit_tests = b.addTest(.{
    //         .root_module = exe_mod1,
    //     });

    //     const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);

    //     // Similar to creating the run step earlier, this exposes a `test` step to
    //     // the `zig build --help` menu, providing a way for the user to request
    //     // running the unit tests.
    //     const test_step = b.step("test", "Run unit tests");
    //     // test_step.dependOn(&run_lib_unit_tests.step);
    //     test_step.dependOn(&run_exe_unit_tests.step);
}
