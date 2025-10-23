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
let value_to_string = function
    | Types.Integer n     -> string_of_int n
    | Types.Boolean true  -> ":true:"
    | Types.Boolean false -> ":false:"
    | Types.String  s     -> s
    | Types.Name    x     -> x
    | Types.Unit          -> ":unit:"
    | Types.Error         -> ":error:"



(** [pp_value ppf v] pretty-prints a [value] to the formatter [ppf].

    This is a utility function designed to pretty-print non-recursive values of
    the type [Types.value] using the **`Fmt`** library. It's typically used for
    debugging, logging, or generating human-readable output.

    @param ppf The pretty-printing formatter to output to.
    @param value The [Types.value] to pretty-print.

    - [Types.Integer i] is printed as {"Integer(\%d)"} (e.g., "Integer(42)").
    - [Types.Boolean b] is printed as {"Boolean(\%b)"} (e.g., "Boolean(true)").
    - [Types.String s] is printed as {"String(\%a)"} using [Fmt.Dump.string]
      for proper quoting (e.g., 'String("hello")').
    - [Types.Name n] is printed as {"Name(\%a)"} using [Fmt.Dump.string] (e.g., 'Name("x")').
    - [Types.Unit] is printed as {"Unit"}.
    - [Types.Error] is printed as {"Error"}.
*)
let pp_value ppf = function
    | Types.Integer i -> Fmt.pf ppf "Integer(%d)" i
    | Types.Boolean b -> Fmt.pf ppf "Boolean(%b)" b
    | Types.String  s -> Fmt.pf ppf "String(%a)" Fmt.Dump.string s
    | Types.Name    n -> Fmt.pf ppf "Name(%a)" Fmt.Dump.string n
    | Types.Unit      -> Fmt.string ppf "Unit"
    | Types.Error     -> Fmt.string ppf "Error"



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
let rec stack_to_string ?(separator = " ") = function
    | []           -> ""
    | [ head ]     -> value_to_string head
    | head :: tail -> value_to_string head ^ separator ^ stack_to_string ~separator tail



(** [pp_stack ppf s] pretty-prints a [stack] to the formatter [ppf].

    This is a utility function designed to pretty-print a [stack] using
    the {Fmt} library.

    @param ppf The pretty-printing formatter to output to.
    @param stack The [stack] to pretty-print.
*)
let pp_stack = Fmt.Dump.list pp_value
