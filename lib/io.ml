(** [pp_value v] pretty-prints a [value] to a string.

    Utility function for converting a [value] to a string for display.
    Refer to Assignment 2 -> TestFOnIntList -> pp_printable

    @param value The [Types.value] to to pretty-print.
    @return [string]
    
    - [Types.Integer n] is printed as its numeric value (e.g., "123").
    - [Types.Boolean true] is printed as ":true:".
    - [Types.Boolean false] is printed as ":false:".
    - [Types.String s] is printed as its raw string content.
    - [Types.Name x] is printed as the name string.
    - [Types.Unit] is printed as ":unit:".
    - [Types.Error] is printed as ":error:".
*)
let value_to_string = function
    | Types.Integer n     -> string_of_int n
    | Types.Boolean true  -> ":true:"
    | Types.Boolean false -> ":false:"
    | Types.String  s     -> s
    | Types.Name    x     -> x
    | Types.Unit          -> ":unit:"
    | Types.Error         -> ":error:"



(** [pp_stack ?separator stack] pretty-prints a [Types.stack] to a string.

    A utility function for converting a [stack] into a readable
    string representation.

    Each element of the [stack] is converted to a string using the [pp_value]
    function. These string representations are then concatenated, separated by
    the specified [separator] string.

    @param separator The optional string to use between elements (defaults = ` `(space)).
    @param stack The [Types.stack] to to pretty-print.
    @return A [string] representation of the stack.
*)
let rec stack_to_string ?(separator = " ") = function
    | []           -> ""
    | [ head ]     -> value_to_string head
    | head :: tail -> value_to_string head ^ separator ^ stack_to_string ~separator tail
