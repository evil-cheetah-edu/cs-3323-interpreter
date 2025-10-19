type value =
    | Integer of int
    | Boolean of bool
    | String  of string
    | Name    of string
    | Unit
    | Error

(** Stack that contains program state *)
type stack = value list
