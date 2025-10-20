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

