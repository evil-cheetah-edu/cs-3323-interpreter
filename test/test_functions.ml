open Test_common


module To_Test = struct
    let push = Interpreter.Functions.push
    let pop = Interpreter.Functions.pop
end


module TestPush = struct
    type stack_test = {
        name:          string;
        expected:      T.stack;
        value:         string;
        initial_state: T.stack
    }

    let check_push ~name ~expected ~value ~initial_state =
        Alcotest.(check (Alcotest.testable IO.pp_stack eq_stack))
            name expected (To_Test.push value initial_state)
    
    let run_test { name; expected; value; initial_state } =
        check_push ~name ~expected ~value ~initial_state
    
    let test_integers () =
        let tests: stack_test list = [
            (* With Empty Stack *)
            {
                name          = "should return `[Integer 33]`: with `push 33` on empty stack";
                expected      = [Integer 33];
                value         = "33";
                initial_state = []
            };
            {
                name          = "should return `[Integer 2002]`: with `push 2002` on empty stack";
                expected      = [Integer 2002];
                value         = "2002";
                initial_state = []
            };
            {
                name          = "should return `[Integer -76]`: with `push -76` on empty stack";
                expected      = [Integer (-76)];
                value         = "-76";
                initial_state = []
            };
            {
                name          = "should return `[Integer 0]`: with `push -0` on empty stack";
                expected      = [Integer 0];
                value         = "-0";
                initial_state = []
            };
            {
                name          = "should return `[Integer 753]`: with `push 753` on empty stack";
                expected      = [Integer 753];
                value         = "753";
                initial_state = []
            };
            {
                name          = "should return `[Error]`: with `push 1_000` on empty stack";
                expected      = [Integer 1000];
                value         = "1_000";
                initial_state = []
            };
            {
                name          = "should return `[Error]`: with `push -10_730` on empty stack";
                expected      = [Integer (-10730)];
                value         = "-10_730";
                initial_state = []
            };
            {
                name          = "should return `[Error, Error]`: with `push 4_433_114` on stack [Error]";
                expected      = [Integer 4433114; Error];
                value         = "4_433_114";
                initial_state = [Error]
            };

            (* With Non-Empty Stack *)
            {
                name          = "should return `[Integer 13; Integer 77]`: with `push 13` on stack [Integer 77]";
                expected      = [Integer 13; Integer 77];
                value         = "13";
                initial_state = [Integer 77]
            };
            {
                name          = {|should return `[Integer 343; String "Hello"]`: with `push 343` on stack [String "Hello"]|};
                expected      = [Integer 343; String "Hello"];
                value         = "343";
                initial_state = [String "Hello"]
            };
            {
                name          = {|should return `[Integer 202; String "Another"; Error]`: with `push 202` on stack [String "Another"; Error]|};
                expected      = [Integer 202; String "Another"; Error];
                value         = "202";
                initial_state = [String "Another"; Error]
            };
        ]
        in
        List.iter run_test tests
    
    let test_invalid_integers () =
        let tests: stack_test list = [
            (* Invalid by Specs: Floats *)
            {
                name          = "should return `[Error]`: with `push 2.5001` on empty stack";
                expected      = [Error];
                value         = "2.5001";
                initial_state = []
            };
            {
                name          = "should return `[Error]`: with `push -10.7` on empty stack";
                expected      = [Error];
                value         = "-10.7";
                initial_state = []
            };
            {
                name          = "should return `[Error, Error]`: with `push -132.50` on stack [Error]";
                expected      = [Error; Error];
                value         = "-132.50";
                initial_state = [Error]
            };
            {
                name          = "should return `[Error, Error]`: with `push -777.510` on stack [Error]";
                expected      = [Error; Error];
                value         = "-777.510";
                initial_state = [Error]
            };

            (* Invalid: Scientific Notation *)
            {
                name          = "should return `[Error]`: with `push 1.23e10` on empty stack";
                expected      = [Error];
                value         = "1.23e10";
                initial_state = []
            };
            {
                name          = "should return `[Error]`: with `push -9.81e-13` on empty stack";
                expected      = [Error];
                value         = "-9.81e-13";
                initial_state = []
            };
            {
                name          = "should return `[Error, Error]`: with `push 9.99e+53` on stack [Error]";
                expected      = [Error; Error];
                value         = "9.99e+53";
                initial_state = [Error]
            };
        ]
        in
        List.iter run_test tests

    let test_booleans () =
        let tests: stack_test list = [
            (* With Empty Stack *)
            {
                name          = "should return `[Boolean true]`: with `push :true:` on empty stack";
                expected      = [Boolean true];
                value         = ":true:";
                initial_state = []
            };
            {
                name          = "should return `[Boolean false]`: with `push :false:` on empty stack";
                expected      = [Boolean false];
                value         = ":false:";
                initial_state = []
            };

            (* With Non-Empty Stack *)
            {
                name          = "should return `[Boolean true; Boolean true]`: with `push :true:` on stack [Boolean true]";
                expected      = [Boolean true; Boolean true];
                value         = ":true:";
                initial_state = [Boolean true]
            };
            {
                name          = "should return `[Boolean true; Boolean false]`: with `push :true:` on stack [Boolean false]";
                expected      = [Boolean true; Boolean false];
                value         = ":true:";
                initial_state = [Boolean false]
            };
            {
                name          = "should return `[Boolean false; Boolean true]`: with `push :false:` on stack [Boolean true]";
                expected      = [Boolean false; Boolean true];
                value         = ":false:";
                initial_state = [Boolean true]
            };
            {
                name          = "should return `[Boolean false; Boolean false]`: with `push :false:` on stack [Boolean false]";
                expected      = [Boolean false; Boolean false];
                value         = ":false:";
                initial_state = [Boolean false]
            };
            {
                name          = "should return `[Boolean false; Error]`: with `push :false:` on stack [Error]";
                expected      = [Boolean false; Error];
                value         = ":false:";
                initial_state = [Error]
            };
        ]
        in
        List.iter run_test tests
    
    let test_invalid_booleans () =
        let tests: stack_test list = [
            (* Not Fully Wrapped with `:` *)
            {
                name          = "should return `[Error]`: with `push :true` on empty stack";
                expected      = [Error];
                value         = ":true";
                initial_state = []
            };
            {
                name          = "should return `[Error]`: with `push true:` on empty stack";
                expected      = [Error];
                value         = "true:";
                initial_state = []
            };
            {
                name          = "should return `[Error]`: with `push :false` on empty stack";
                expected      = [Error];
                value         = ":false";
                initial_state = []
            };
            {
                name          = "should return `[Error]`: with `push false:` on empty stack";
                expected      = [Error];
                value         = "false:";
                initial_state = []
            };

            (* Invalid by Specs: Title Case *)
            {
                name          = "should return `[Error]`: with `push :True:` on empty stack";
                expected      = [Error];
                value         = ":True:";
                initial_state = []
            };
            {
                name          = "should return `[Error]`: with `push :True:` on empty stack";
                expected      = [Error];
                value         = ":True:";
                initial_state = []
            };
            {
                name          = "should return `[Error]`: with `push :False:` on empty stack";
                expected      = [Error];
                value         = ":False:";
                initial_state = []
            };
            {
                name          = "should return `[Error]`: with `push :False:` on empty stack";
                expected      = [Error];
                value         = ":False:";
                initial_state = []
            };

            (* Invalid by Specs: Uppercase *)
            {
                name          = "should return `[Error]`: with `push :TRUE:` on empty stack";
                expected      = [Error];
                value         = ":TRUE:";
                initial_state = []
            };
            {
                name          = "should return `[Error]`: with `push :FALSE:` on empty stack";
                expected      = [Error];
                value         = ":FALSE:";
                initial_state = []
            };

            (* Invalid by Specs: Numeric Values *)
            {
                name          = "should return `[Error]`: with `push :1:` on empty stack";
                expected      = [Error];
                value         = ":1:";
                initial_state = []
            };
            {
                name          = "should return `[Error]`: with `push :0:` on empty stack";
                expected      = [Error];
                value         = ":0:";
                initial_state = []
            };

            (* Invalid by Specs: Not Wrapped with `:` result in Name *)
            {
                name          = {|should return `[Name "True"]`: with `push True` on empty stack|};
                expected      = [Name "True"];
                value         = "True";
                initial_state = []
            };
            {
                name          = {|should return `[Name "False"]`: with `push False` on empty stack|};
                expected      = [Name "False"];
                value         = "False";
                initial_state = []
            };
            {
                name          = {|should return `[Name "true"]`: with `push true` on empty stack|};
                expected      = [Name "true"];
                value         = "true";
                initial_state = []
            };
            {
                name          = {|should return `[Name false]`: with `push false` on empty stack|};
                expected      = [Name "false"];
                value         = "false";
                initial_state = []
            };
        ]
        in
        List.iter run_test tests


    let test_strings () =
        let tests: stack_test list = [
            (* With Empty Stack *)
            {
                name          = {|should return `[String "something"]`: with `push "something"` on empty stack|};
                expected      = [String "something"];
                value         = {|"something"|};
                initial_state = []
            };
            {
                name = (
                    {|should return `[String "pneumonoultramicroscopicsilicovolcanoconiosis"]`: |} ^
                    {|with `push "pneumonoultramicroscopicsilicovolcanoconiosis"` on empty stack|}
                );
                expected      = [String "pneumonoultramicroscopicsilicovolcanoconiosis"];
                value         = {|"pneumonoultramicroscopicsilicovolcanoconiosis"|};
                initial_state = []
            };

            (* With Non-Empty Stack *)
            {
                name          = {|should return `[String "Hello"; String "there"]`: with `push "there"` on stack [String "there"]|};
                expected      = [String "Hello"; String "there"];
                value         = {|"Hello"|};
                initial_state = [String "there"]
            };
            {
                name          = {|should return `[String "This is "; Boolean false]`: with `push "This is "` on stack [Boolean false]|};
                expected      = [String "This is "; Boolean false];
                value         = {|"This is "|};
                initial_state = [Boolean false]
            };
            {
                name          = {|should return `[String "false"; Unit; Error]`: with `push "false"` on stack [Unit; Error]|};
                expected      = [String "false"; Unit; Error];
                value         = {|"false"|};
                initial_state = [Unit; Error]
            };
        ]
        in
        List.iter run_test tests

    let test_invalid_strings () =
        let tests: stack_test list = [
            (* Not Fully Wrapped with {| " |} *)
            {
                name          = {|should return `[Error]`: with `push "Hello World` on empty stack|};
                expected      = [Error];
                value         = {|"Hello World|};
                initial_state = []
            };
            {
                name          = {|should return `[Error]`: with `push Hello World"` on empty stack|};
                expected      = [Error];
                value         = {|Hello World"|};
                initial_state = []
            };
            {
                name          = {|should return `[Error]`: with `push Hello World` on empty stack|};
                expected      = [Error];
                value         = {|Hello World|};
                initial_state = []
            };
        ]
        in
        List.iter run_test tests
    
    let test_names () =
        let tests: stack_test list = [
            (* With Empty Stack *)
            {
                name          = {|should return `[Name "x"]`: with `push x` on empty stack|};
                expected      = [Name "x"];
                value         = "x";
                initial_state = []
            };
            {
                name          = {|"should return `[Name "alpha"]`: with `push alpha` on empty stack|};
                expected      = [Name "alpha"];
                value         = "alpha";
                initial_state = []
            };
            {
                name          = {|"should return `[Name "omega3"]`: with `push omega3` on empty stack|};
                expected      = [Name "omega3"];
                value         = "omega3";
                initial_state = []
            };
            {
                name          = {|"should return `[Name "b6"]`: with `push b6` on empty stack|};
                expected      = [Name "b6"];
                value         = "b6";
                initial_state = []
            };
            {
                name          = {|"should return `[Name "undefined_behavior"]`: with `push undefined_behavior` on empty stack|};
                expected      = [Name "undefined_behavior"];
                value         = "undefined_behavior";
                initial_state = []
            };

            (* With Non-Empty Stack *)
            {
                name = (
                    {|should return `[Name "__init__"; Error; Unit; String "Debug"]`: |} ^
                    {|with `push __init__` on stack [String "there"]|}
                );
                expected      = [Name "__init__"; Error; Unit; String "Debug"];
                value         = "__init__";
                initial_state = [Error; Unit; String "Debug"]
            };
            {
                name = (
                    {|should return `[Name "_magic_"; Integer 8; Name "a"]`: |} ^
                    {|with `push _magic_` on stack [String "there"]|}
                );
                expected      = [Name "_magic_"; Integer 8; Name "a"];
                value         = "_magic_";
                initial_state = [Integer 8; Name "a"]
            };
            {
                name = (
                    {|should return `[Name "var2"; Name "var1"; Name "var0"]`: |} ^
                    {|with `push var1` on stack [Name "var1"; Name "var0"]|}
                );
                expected      = [Name "var2"; Name "var1"; Name "var0"];
                value         = "var2";
                initial_state = [Name "var1"; Name "var0"]
            };
        ]
        in
        List.iter run_test tests

    let test_units () =
        let tests: stack_test list = [
            (* With Empty Stack *)
            {
                name          = {|should return `[Unit]`: with `push :unit:` on empty stack|};
                expected      = [Unit];
                value         = ":unit:";
                initial_state = []
            };

            (* With Non-Empty Stack *)
            {
                name = (
                    {|should return `[Name "__init__"; Error; Unit; String "Debug"]`: |} ^
                    {|with `push __init__` on stack [String "there"]|}
                );
                expected      = [Name "__init__"; Error; Unit; String "Debug"];
                value         = "__init__";
                initial_state = [Error; Unit; String "Debug"]
            };
            {
                name = (
                    {|should return `[Name "_magic_"; Integer 8; Name "a"]`: |} ^
                    {|with `push _magic_` on stack [String "there"]|}
                );
                expected      = [Name "_magic_"; Integer 8; Name "a"];
                value         = "_magic_";
                initial_state = [Integer 8; Name "a"]
            };
            {
                name = (
                    {|should return `[Name "var2"; Name "var1"; Name "var0"]`: |} ^
                    {|with `push var1` on stack [Name "var1"; Name "var0"]|}
                );
                expected      = [Name "var2"; Name "var1"; Name "var0"];
                value         = "var2";
                initial_state = [Name "var1"; Name "var0"]
            };
        ]
        in
        List.iter run_test tests

    let test_invalid_units () =
        let tests: stack_test list = [
            (* Not Fully Wrapped with `:` *)
            {
                name          = "should return `[Error]`: with `push :unit` on empty stack";
                expected      = [Error];
                value         = ":unit";
                initial_state = []
            };
            {
                name          = "should return `[Error]`: with `push unit:` on empty stack";
                expected      = [Error];
                value         = "unit:";
                initial_state = []
            };

            (* Invalid by Specs: Title Case *)
            {
                name          = "should return `[Error]`: with `push :Unit:` on empty stack";
                expected      = [Error];
                value         = ":Unit:";
                initial_state = []
            };

            (* Invalid by Specs: Uppercase *)
            {
                name          = "should return `[Error]`: with `push :UNIT:` on empty stack";
                expected      = [Error];
                value         = ":UNIT:";
                initial_state = []
            };


            (* Invalid by Specs: Unit Notation (Empty Tuple) *)
            {
                name          = "should return `[Error]`: with `push ()` on empty stack";
                expected      = [Error];
                value         = "()";
                initial_state = []
            };
            {
                name          = "should return `[Error]`: with `push :():` on empty stack";
                expected      = [Error];
                value         = ":():";
                initial_state = []
            };

            (* Invalid by Specs: Not Wrapped with `:` result in Name *)
            {
                name          = {|should return `[Name unit]`: with `push unit` on empty stack|};
                expected      = [Name "unit"];
                value         = "unit";
                initial_state = []
            };
            {
                name          = {|should return `[Name Unit]`: with `push Unit` on empty stack|};
                expected      = [Name "Unit"];
                value         = "Unit";
                initial_state = []
            };
            {
                name          = {|should return `[Name UNIT]`: with `push UNIT` on empty stack|};
                expected      = [Name "UNIT"];
                value         = "UNIT";
                initial_state = []
            };
        ]
        in
        List.iter run_test tests

    let test_errors () =
        let tests: stack_test list = [
            (* With Empty Stack *)
            {
                name          = "should return `[Error]`: with `push :error:` on empty stack";
                expected      = [Error];
                value         = ":error:";
                initial_state = []
            };

            (* With Non-Empty Stack *)
            {
                name          = "should return `[Error; Boolean true]`: with `push :error:` on stack [Boolean true]";
                expected      = [Error; Boolean true];
                value         = ":error:";
                initial_state = [Boolean true]
            };
            {
                name          = {|should return `[Error; Name "Pittsburgh"]`: with `push :error:` on stack [Name "Pittsburgh"]|};
                expected      = [Error; Name "Pittsburgh"];
                value         = ":error:";
                initial_state = [Name "Pittsburgh"]
            };
            {
                name          = "should return `[Error; Unit; Unit; Error]`: with `push :error:` on stack [Unit; Unit; Error]";
                expected      = [Error; Unit; Unit; Error];
                value         = ":error:";
                initial_state = [Unit; Unit; Error]
            };
        ]
        in
        List.iter run_test tests

    let assignment_spec_examples () =
        (* Spec Test 1: 4.1.1 - Pushing Integers to the Stack - Pushing Integers *)
        let () =
            (* 0. Initialize state *)
            let current_stack = ref [] in

            (* 1. Check if current stack is empty *)
            check_empty ~name:"Spec Test 1: initial stack should be empty" ~value:!current_stack;

            (* 2. Check Push with value "5"; current stack [] *)
            let int_input = "5" in
            let expected = [T.Integer 5] in
            check_push
                ~name:"Spec Test 1: should push Integer 5 on stack []"
                ~expected
                ~value:int_input
                ~initial_state:!current_stack;
            current_stack := To_Test.push int_input !current_stack;

            (* 3. Check push with value "-0"; current stack [Integer 5] *)
            let int_input = "-0" in
            let expected = [T.Integer 0; T.Integer 5] in
            check_push
                ~name:"Spec Test 1: should push Integer -0 on stack [Integer 5]"
                ~expected
                ~value:int_input
                ~initial_state:!current_stack;
            current_stack := To_Test.push int_input !current_stack;
        in

        (* Spec Test 2: 4.1.2 - Pushing Strings to the Stack - Regular Strings *)
        let () =
            (* 0. Initialize state *)
            let current_stack = ref [] in

            (* 1. Check if current stack is empty *)
            check_empty ~name:"Spec Test 2: initial stack should be empty" ~value:!current_stack;

            (* 2. Check Push with value '"deadpool"'; current stack [] *)
            let string_input = {|"deadpool"|} in
            let expected = [T.String "deadpool"] in
            check_push
                ~name:{|Spec Test 2: should push String "deadpool" on stack []|}
                ~expected
                ~value:string_input
                ~initial_state:!current_stack;
            current_stack := To_Test.push string_input !current_stack;

            (* 3. Check push with value '"batman"'; current stack [String "deadpool"] *)
            let string_input = {|"batman"|} in
            let expected = [T.String "batman"; T.String "deadpool"] in
            check_push
                ~name:{|Spec Test 2: should push String "batman" on stack [String "deadpool"]|}
                ~expected
                ~value:string_input
                ~initial_state:!current_stack;
            current_stack := To_Test.push string_input !current_stack;

            (* 4. Check push with value '"this is a string"'; current stack [String "batman"; String "deadpool"] *)
            let string_input = {|"this is a string"|} in
            let expected = [T.String "this is a string"; T.String "batman"; T.String "deadpool"] in
            check_push
                ~name:{|Spec Test 2: should push String "this is a string" on stack [String "batman"; String "deadpool"]|}
                ~expected
                ~value:string_input
                ~initial_state:!current_stack;
            current_stack := To_Test.push string_input !current_stack;
        in

        (* Spec Test 3: 4.1.2 - Pushing Strings to the Stack - Strings with Leading or Trailing Spaces *)
        let () =
            (* 0. Initialize state *)
            let current_stack = ref [] in

            (* 1. Check if current stack is empty *)
            check_empty ~name:"Spec Test 3: initial stack should be empty" ~value:!current_stack;

            (* 2. Check Push with value '" deadp ool "'; current stack [] *)
            let string_input = {|" deadp ool "|} in
            let expected = [T.String " deadp ool "] in
            check_push
                ~name:{|Spec Test 3: should push String " deadp ool " on stack []|}
                ~expected
                ~value:string_input
                ~initial_state:!current_stack;
            current_stack := To_Test.push string_input !current_stack;

            (* 3. Check push with value '"this is a string  "'; current stack [String " deadp ool "] *)
            let string_input = {|"this is a string  "|} in
            let expected = [T.String "this is a string  "; T.String " deadp ool "] in
            check_push
                ~name:{|Spec Test 3: should push String "this is a string  " on stack [String " deadp ool "]|}
                ~expected
                ~value:string_input
                ~initial_state:!current_stack;
            current_stack := To_Test.push string_input !current_stack;
        in

        (* Spec Test 4: 4.2 Pushing Names to the Stack - Example 1 *)
        let () =
            (* 0. Initialize state *)
            let current_stack = ref [] in

            (* 1. Check if current stack is empty *)
            check_empty ~name:"Spec Test 4: initial stack should be empty" ~value:!current_stack;

            (* 2. Check Push with value "a"; current stack [] *)
            let name_input = "a" in
            let expected = [T.Name "a"] in
            check_push
                ~name:{|Spec Test 4: should push Name "a" on stack []|}
                ~expected
                ~value:name_input
                ~initial_state:!current_stack;
            current_stack := To_Test.push name_input !current_stack;

            (* 3. Check Push with value "13"; current stack [Name "a"] *)
            let int_input = "13" in
            let expected = [T.Integer 13; T.Name "a"] in
            check_push
                ~name:{|Spec Test 4: should push Integer "13" on stack [Name "a"]|}
                ~expected
                ~value:int_input
                ~initial_state:!current_stack;
            current_stack := To_Test.push int_input !current_stack;
        in

        (* Spec Test 5: 4.2 Pushing Names to the Stack - Example 2 *)
        let () =
            (* 0. Initialize state *)
            let current_stack = ref [] in

            (* 1. Check if current stack is empty *)
            check_empty ~name:"Spec Test 5: initial stack should be empty" ~value:!current_stack;

            (* 2. Check Push with value "__name1__"; current stack [] *)
            let name_input = "__name1__" in
            let expected = [T.Name "__name1__"] in
            check_push
                ~name:{|Spec Test 5: should push Name "__name1__" on stack []|}
                ~expected
                ~value:name_input
                ~initial_state:!current_stack;
            current_stack := To_Test.push name_input !current_stack;

            (* 3. Check Push with value "3"; current stack [Name "__name1__"] *)
            let int_input = "3" in
            let expected = [T.Integer 3; T.Name "__name1__"] in
            check_push
                ~name:{|Spec Test 5: should push Integer "3" on stack [Name "__name1__"]|}
                ~expected
                ~value:int_input
                ~initial_state:!current_stack;
            current_stack := To_Test.push int_input !current_stack;
        in

        (* Spec Test 6: 4.3 booleans - Example with :true: *)
        let () =
            (* 0. Initialize state *)
            let current_stack = ref [] in

            (* 1. Check if current stack is empty *)
            check_empty ~name:"Spec Test 6: initial stack should be empty" ~value:!current_stack;

            (* 2. Check Push with value "5"; current stack [] *)
            let int_input = "5" in
            let expected = [T.Integer 5] in
            check_push
                ~name:{|Spec Test 6: should push Integer 5 on stack []|}
                ~expected
                ~value:int_input
                ~initial_state:!current_stack;
            current_stack := To_Test.push int_input !current_stack;

            (* 3. Check Push with value "3"; current stack [Integer 5] *)
            let bool_input = ":true:" in
            let expected = [T.Boolean true; T.Integer 5] in
            check_push
                ~name:{|Spec Test 6: should push Boolean true on stack [Integer 5]|}
                ~expected
                ~value:bool_input
                ~initial_state:!current_stack;
            current_stack := To_Test.push bool_input !current_stack;
        in

        (* Spec Test 7: 4.4 error and unit - Example with multiple :error: and :unit: *)
        let () =
            (* 0. Initialize state *)
            let current_stack = ref [] in

            (* 1. Check if current stack is empty *)
            check_empty ~name:"Spec Test 7: initial stack should be empty" ~value:!current_stack;

            (* 2. Check Push with value ":error:"; current stack [] *)
            let error_input = ":error" in
            let expected = [T.Error] in
            check_push
                ~name:"Spec Test 7: should push Error on stack []"
                ~expected
                ~value:error_input
                ~initial_state:!current_stack;
            current_stack := To_Test.push error_input !current_stack;

            (* 3. Check Push with value ":unit:"; current stack [Error] *)
            let unit_input = ":unit:" in
            let expected = [T.Unit; T.Error] in
            check_push
                ~name:"Spec Test 7: should push Unit on stack [Error]"
                ~expected
                ~value:unit_input
                ~initial_state:!current_stack;
            current_stack := To_Test.push unit_input !current_stack;

            (* 4. Check Push with value ":error:"; current stack [Unit; Error] *)
            let error_input = ":error" in
            let expected = [T.Error; T.Unit; T.Error] in
            check_push
                ~name:"Spec Test 7: should push Error on stack [Unit; Error]"
                ~expected
                ~value:error_input
                ~initial_state:!current_stack;
            current_stack := To_Test.push error_input !current_stack;

            (* 5. Check Push with value ":unit:"; current stack [Error; Unit; Error] *)
            let unit_input = ":unit:" in
            let expected = [T.Unit; T.Error; T.Unit; T.Error] in
            check_push
                ~name:"Spec Test 7: should push Unit on stack [Error; Unit; Error]"
                ~expected
                ~value:unit_input
                ~initial_state:!current_stack;
            current_stack := To_Test.push unit_input !current_stack;

            (* 6. Check Push with value ":unit:"; current stack [Unit; Error; Unit; Error] *)
            let unit_input = ":unit:" in
            let expected = [T.Unit; T.Unit; T.Error; T.Unit; T.Error] in
            check_push
                ~name:"Spec Test 7: should push Unit on stack [Unit; Error; Unit; Error]"
                ~expected
                ~value:unit_input
                ~initial_state:!current_stack;
            current_stack := To_Test.push unit_input !current_stack;
        in
        
        ()
end


let suites: unit Alcotest.test list = [
    ("Functions/push", [
        Alcotest.test_case "integers (valid)"          `Quick TestPush.test_integers;
        Alcotest.test_case "integers (invalid)"        `Quick TestPush.test_invalid_integers;
        Alcotest.test_case "booleans (valid)"          `Quick TestPush.test_booleans;
        Alcotest.test_case "booleans (invalid)"        `Quick TestPush.test_invalid_booleans;
        Alcotest.test_case "strings"                   `Quick TestPush.test_strings;
        Alcotest.test_case "strings (invalid)"         `Quick TestPush.test_invalid_strings;
        Alcotest.test_case "names"                     `Quick TestPush.test_names;
        Alcotest.test_case "units (valid)"             `Quick TestPush.test_units;
        Alcotest.test_case "units (invalid)"           `Quick TestPush.test_invalid_units;
        Alcotest.test_case "errors"                    `Quick TestPush.test_errors;
        Alcotest.test_case "assignment spec examples"  `Quick TestPush.assignment_spec_examples
    ]);

    ("Functions/pop", [

    ]);
]