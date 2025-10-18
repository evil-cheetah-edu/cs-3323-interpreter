let resolve_int (value: Types.value): int option =
    match value with
    | Integer number -> Some number
    | _              -> None
