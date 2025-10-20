(** Add the top two integer values on the stack.
    - If the stack starts with two values [left] and [right] that both
      resolve to integers via [Utils.resolve_int], they are popped and
      their sum is pushed:
      {[ Types.Integer (left + right) :: rest ]}
      
    - If either value does not resolve to an integer, the operands are
      {NOT} consumed; [Types.Error] is pushed on top and the previous
      values are left in place:
      {[ Types.Error :: left :: right :: rest ]}
    
    - With fewer than two elements, an error is signaled:
      {[ Types.Error :: item :: rest ]}
      {[ Types.Error :: [] ]}
    
    @param stack Current stack
    @return The updated stack with result of {add} at the front
 *)
let add (stack: Types.stack): Types.stack =
    match stack with
    | left :: right :: rest -> (
        match Utils.resolve_int left, Utils.resolve_int right with
        | Some x, Some y -> Types.Integer (x + y) :: rest
        | _              -> Types.Error :: left :: right :: rest
    )
    | item :: rest          -> Types.Error :: item :: rest
    | []                    -> [Types.Error]


(** Push a value parsed from [argument] onto [stack].

    The token in [argument] is interpreted in this order:
    - {":true:"} pushes [Types.Boolean true]
    - {":false:"} pushes [Types.Boolean false]
    - {":unit:"} pushes [Types.Unit]
    - A double-quoted string pushes [Types.String s] without outer quotes
    - An integer literal pushes [Types.Integer i]
    - If [Utils.is_valid_name argument] → pushes [Types.Name var]
    - Otherwise If none of the cases match, [Error] is pushed.

    @param argument Raw token to interpret.
    @param stack Current stack.
    @return The updated stack with the parsed value consed at the front.
*)
let push (argument: string) (stack: Types.stack): Types.stack =
    match argument with
    | ":true:"  -> (Types.Boolean  true) :: stack
    | ":false:" -> (Types.Boolean false) :: stack
    | ":unit:"  ->          (Types.Unit) :: stack

    | _ when String.length argument >= 2
        && String.starts_with ~prefix:"\"" argument
        && String.ends_with   ~suffix:"\"" argument ->
            (Types.String (String.sub argument 1 (String.length argument - 2))) :: stack

    (* TODO(eugene): avoid double parsing *)
    | _ when Option.is_some (int_of_string_opt argument) ->
            (Types.Integer (int_of_string argument)) :: stack

    | _ when Utils.is_valid_name argument ->
            (Types.Name argument) :: stack

    | _ ->
        Error :: stack
