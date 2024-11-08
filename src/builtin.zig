const object = @import("object.zig");

pub const NULL = object.Null{};

pub var NULL_OBJECT = object.Object{ .null_ = NULL };

pub const TRUE = object.Boolean{ .value = true };
pub const FALSE = object.Boolean{ .value = false };

pub var TRUE_OBJECT = object.Object{ .boolean = TRUE };
pub var FALSE_OBJECT = object.Object{ .boolean = FALSE };
