(** [resolve_int value] attempts to extract the [int] carried by the [Types.Integer].
    @param value The [Types.value] to inspect.
    @return [Some n] if [value] is [Types.Integer n]; [None] for any other type.
    
    Examples:
    {[
      resolve_int (Integer 42) = Some 42;
      resolve_int (String "42") = None;
      resolve_int Unit = None;
    ]}
*)
let resolve_int (value: Types.value): int option =
    match value with
    | Integer number -> Some number
    | _              -> None



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
let pp_value = function
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
let rec pp_stack ?(separator = " ") = function
    | []           -> ""
    | [ head ]     -> pp_value head
    | head :: tail -> pp_value head ^ separator ^ pp_stack tail



(** [is_valid_name variable_name] checks if a string is a legal variable name
    according to the Assignment Specifications.

    A valid variable name must follow to the regular expression:
    "^[_]*[a-zA-Z][a-zA-Z_]*$"

    Explanation:
    - Start with zero or more underscores
    - Immediately followed by at least one {letter}
    - Be composed only of letters, numbers and underscores for the remainder
    of the string

    @param variable_name [string] to check if its name is valid
    @return [true] if name is valid, [false] otherwise

    For more information on legal constant and variable names, please, refer
    to the {Assignment Specifications, Section 3.1 Constants}.
*)
let is_valid_name (variable_name: string): bool =
    let pattern = Str.regexp "^[_]*[a-zA-Z][0-9a-zA-Z_]*$" in
    try
        let _ = Str.search_forward pattern variable_name 0 in
        true
    with
    | Not_found -> false
