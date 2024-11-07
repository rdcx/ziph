const std = @import("std");
const token = @import("token.zig");
const String = @import("string.zig").String;

// Test initialization
test {
    std.testing.refAllDecls(@This());
}

pub const Program = struct {
    statements: std.ArrayList(Statement),

    pub fn toString(self: *Program, buf: *String) !void {
        var i: usize = 0;
        while (i < self.statements.items.len) : (i += 1) {
            try self.statements.items[i].toString(buf);
        }
    }

    test "Program toString" {
        var gpa = std.heap.GeneralPurposeAllocator(.{}){};
        const alloc = gpa.allocator();
        const statements = std.ArrayList(Statement).init(alloc);
        statements.append(Statement{ .identifier = Identifier{ .name = "user" } });
        var program = Program{ .statements = statements };

        var buf = String.init(alloc);

        try program.toString(&buf);

        try std.testing.expectEqual(buf.buffer.?, "user");
    }
};

pub const Statement = union(enum) {
    identifier: Identifier,
};

pub const Identifier = struct {
    name: []const u8,

    pub fn toString(self: *Identifier, buf: *String) !void {
        try buf.append(self.name);
    }
};
