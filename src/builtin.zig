const object = @import("object.zig");

pub const NULL = object.Null{};

pub var NULL_OBJECT = object.Object{ .null_ = NULL };
