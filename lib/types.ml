type value =
    | Integer of int
    | Boolean of bool
    | String  of string
    | Name    of string
    | Unit
    | Error

type stack = value list
