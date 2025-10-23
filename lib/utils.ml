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
