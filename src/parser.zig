const std = @import("std");
const testing = std.testing;

test "dummy" {
    try testing.expect(1 == 1);

    try testing.expect(2 == 1);
}
