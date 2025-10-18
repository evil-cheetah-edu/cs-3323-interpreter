let add (stack: Types.stack): Types.stack = 
    match stack with
    | left :: right :: rest -> (
        match Utils.resolve_int left, Utils.resolve_int right with   
        | Some x, Some y -> Types.Integer (x + y) :: rest
        | _              -> Types.Error :: left :: right :: rest
    )
    | item :: rest          -> Types.Error :: item :: rest
    | []                    -> [Types.Error]
