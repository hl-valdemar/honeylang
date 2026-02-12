const std = @import("std");
const mem = @import("std").mem;

const SourceIndex = @import("../source/source.zig").SourceIndex;
const SourceCode = @import("../source/source.zig").SourceCode;

pub const Severity = enum { fatal, err, warning };

pub const ErrorInfo = struct {
    code: []const u8,
    message: []const u8,
    help: []const u8,
    severity: Severity = .err,
};

pub const SemanticErrorKind = enum {
    // symbol errors
    unknown_type,
    duplicate_symbol,
    undefined_symbol,

    // type checking errors
    type_mismatch,
    invalid_operand_type,
    cannot_negate_unsigned,
    logical_op_requires_bool,
    arithmetic_op_requires_numeric,
    comparison_requires_compatible,
    argument_count_mismatch,
    argument_type_mismatch,
    return_type_mismatch,
    assignment_to_immutable,
    condition_not_bool,
    not_callable,

    // usage warnings
    unused_variable,
    unused_constant,
    unused_function,

    // type resolution errors
    unresolved_type,
    missing_function_body,
    missing_entry_point,
    duplicate_field,
    no_such_field,
    field_access_on_non_struct,

    // struct literal errors
    missing_field,
    duplicate_literal_field,

    // usage warnings
    unused_type,

    // pointer errors
    cannot_take_address,
    deref_non_pointer,
    assign_through_immutable_ptr,
    pointer_arithmetic,

    pub fn info(self: SemanticErrorKind) ErrorInfo {
        return error_info.get(self);
    }

    pub fn isWarning(self: SemanticErrorKind) bool {
        return error_info.get(self).severity == .warning;
    }
};

/// Single source of truth for all error metadata.
/// Compile-time checked to ensure all error kinds are covered.
pub const error_info = std.EnumArray(SemanticErrorKind, ErrorInfo).init(.{
    .unknown_type = .{
        .code = "S001",
        .message = "unknown type",
        .help = "type not found",
    },
    .duplicate_symbol = .{
        .code = "S002",
        .message = "duplicate symbol",
        .help = "symbol already defined",
    },
    .undefined_symbol = .{
        .code = "S003",
        .message = "undefined symbol",
        .help = "symbol not found in scope",
    },
    .type_mismatch = .{
        .code = "S004",
        .message = "type mismatch",
        .help = "types are not compatible",
    },
    .invalid_operand_type = .{
        .code = "S005",
        .message = "invalid operand type",
        .help = "operand has wrong type",
    },
    .cannot_negate_unsigned = .{
        .code = "S006",
        .message = "cannot negate unsigned integer",
        .help = "negation requires signed type",
    },
    .logical_op_requires_bool = .{
        .code = "S007",
        .message = "logical operation requires boolean operands",
        .help = "operands must be bool",
    },
    .arithmetic_op_requires_numeric = .{
        .code = "S008",
        .message = "arithmetic operation requires numeric operands",
        .help = "operands must be numeric",
    },
    .comparison_requires_compatible = .{
        .code = "S009",
        .message = "comparison requires compatible types",
        .help = "operands must have the same type",
    },
    .argument_count_mismatch = .{
        .code = "S010",
        .message = "argument count mismatch",
        .help = "wrong number of arguments",
    },
    .argument_type_mismatch = .{
        .code = "S011",
        .message = "argument type mismatch",
        .help = "argument has wrong type",
    },
    .return_type_mismatch = .{
        .code = "S012",
        .message = "return type mismatch",
        .help = "returned value doesn't match function signature",
    },
    .assignment_to_immutable = .{
        .code = "S013",
        .message = "cannot assign to immutable variable",
        .help = "use 'mut' to make variable mutable",
    },
    .condition_not_bool = .{
        .code = "S014",
        .message = "condition must be boolean",
        .help = "expected bool expression",
    },
    .not_callable = .{
        .code = "S015",
        .message = "expression is not callable",
        .help = "only functions can be called",
    },
    .unused_variable = .{
        .code = "S016",
        .message = "unused variable",
        .help = "remove or use this variable",
        .severity = .warning,
    },
    .unused_constant = .{
        .code = "S017",
        .message = "unused constant",
        .help = "remove or use this constant",
        .severity = .warning,
    },
    .unresolved_type = .{
        .code = "S018",
        .message = "cannot resolve type",
        .help = "add a type annotation or use in a typed context",
    },
    .unused_function = .{
        .code = "S019",
        .message = "unused function",
        .help = "remove or use this function",
        .severity = .warning,
    },
    .missing_function_body = .{
        .code = "S020",
        .message = "honey function must have a body",
        .help = "only foreign-convention functions (c, cobol, fortran) can be externally defined",
    },
    .missing_entry_point = .{
        .code = "S021",
        .message = "missing entry point",
        .help = "program must define a 'main' function",
        .severity = .fatal,
    },
    .duplicate_field = .{
        .code = "S022",
        .message = "duplicate field",
        .help = "field name already defined in this struct",
    },
    .unused_type = .{
        .code = "S023",
        .message = "unused type",
        .help = "remove or use this type",
        .severity = .warning,
    },
    .no_such_field = .{
        .code = "S024",
        .message = "no such field",
        .help = "field does not exist on this struct",
    },
    .field_access_on_non_struct = .{
        .code = "S025",
        .message = "field access on non-struct type",
        .help = "dot access requires a struct type",
    },
    .missing_field = .{
        .code = "S026",
        .message = "missing field in struct literal",
        .help = "all fields must be initialized",
    },
    .duplicate_literal_field = .{
        .code = "S027",
        .message = "duplicate field in struct literal",
        .help = "field already specified in this literal",
    },
    .cannot_take_address = .{
        .code = "S028",
        .message = "cannot take address of expression",
        .help = "only variables and fields are addressable",
    },
    .deref_non_pointer = .{
        .code = "S029",
        .message = "dereference of non-pointer type",
        .help = "only pointer types can be dereferenced with ^",
    },
    .assign_through_immutable_ptr = .{
        .code = "S030",
        .message = "cannot assign through immutable pointer",
        .help = "use @mut to create a mutable pointer",
    },
    .pointer_arithmetic = .{
        .code = "S031",
        .message = "arithmetic on single-item pointer",
        .help = "single-item pointers do not support arithmetic; use a many-item pointer (*T) instead",
    },
});

pub const SemanticError = struct {
    kind: SemanticErrorKind,
    start: SourceIndex,
    end: SourceIndex,
};

pub const ErrorList = struct {
    allocator: mem.Allocator,
    errors: std.ArrayList(SemanticError),
    warnings: std.ArrayList(SemanticError),

    pub fn init(allocator: mem.Allocator) !ErrorList {
        return .{
            .allocator = allocator,
            .errors = try std.ArrayList(SemanticError).initCapacity(allocator, 0),
            .warnings = try std.ArrayList(SemanticError).initCapacity(allocator, 0),
        };
    }

    pub fn deinit(self: *ErrorList) void {
        self.errors.deinit();
        self.warnings.deinit();
    }

    pub fn add(self: *ErrorList, err: SemanticError) !void {
        if (err.kind.isWarning()) {
            try self.warnings.append(self.allocator, err);
        } else {
            try self.errors.append(self.allocator, err);
        }
    }

    pub fn hasErrors(self: *const ErrorList) bool {
        return self.errors.items.len > 0;
    }

    pub fn hasWarnings(self: *const ErrorList) bool {
        return self.warnings.items.len > 0;
    }

    pub fn errorCount(self: *const ErrorList) usize {
        return self.errors.items.len;
    }

    pub fn warningCount(self: *const ErrorList) usize {
        return self.warnings.items.len;
    }
};
