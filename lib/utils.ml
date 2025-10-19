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
