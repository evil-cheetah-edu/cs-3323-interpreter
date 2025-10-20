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
