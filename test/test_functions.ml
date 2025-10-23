open Test_common


module Utils = Interpreter.Utils
module IO = Interpreter.Io


let empty_stack = []


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
                name          = "should return `[Integer 0]`: with `push 0` on empty stack";
                expected      = [Integer 0];
                value         = "-0";
                initial_state = []
            };
            {
                name          = "should return `[Error]`: with `push 2.5` on empty stack";
                expected      = [Error];
                value         = "2.5";
                initial_state = []
            };
            {
                name          = "should return `[Error]`: with `push -10.7` on empty stack";
                expected      = [Error];
                value         = "-10.7";
                initial_state = []
            };

            (* With Non-Empty Stack *)
            {
                name          = "should return `[Integer 13; Integer 77]`: with `push 13` on stack [Integer 77]";
                expected      = [Integer 13; Integer 77];
                value         = "13";
                initial_state = [Integer 77]
            };
            {
                name          = "should return `[Error, Error]`: with `push -132.5` on stack [Error]";
                expected      = [Error; Error];
                value         = "-132.5";
                initial_state = [Error]
            };
            {
                name          = "should return `[Integer 343; String \"Hello\"]`: with `push 343` on stack [String \"Hello\"]";
                expected      = [Integer 343; String "Hello"];
                value         = "343";
                initial_state = [String "Hello"]
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
    
    let test_strings () =
        let tests: stack_test list = [
            (* With Empty Stack *)
            {
                name          = "should return `[String \"something\"]`: with `push \"something\"` on empty stack";
                expected      = [String "something"];
                value         = "\"something\"";
                initial_state = []
            };
            {
                name = (
                    "should return `[String \"pneumonoultramicroscopicsilicovolcanoconiosis\"]`: " ^
                    "with `push \"pneumonoultramicroscopicsilicovolcanoconiosis\"` on empty stack"
                );
                expected      = [String "pneumonoultramicroscopicsilicovolcanoconiosis"];
                value         = "\"pneumonoultramicroscopicsilicovolcanoconiosis\"";
                initial_state = []
            };

            (* With Non-Empty Stack *)
            {
                name          = "should return `[String \"Hello\"; String \"there\"]`: with `push \"there\"` on stack [String \"there\"]";
                expected      = [String "Hello"; String "there"];
                value         = "\"Hello\"";
                initial_state = [String "there"]
            };
            {
                name          = "should return `[String \"This is \"; Boolean false]`: with `push \"This is \"` on stack [Boolean false]";
                expected      = [String "This is "; Boolean false];
                value         = "\"This is \"";
                initial_state = [Boolean false]
            };
            {
                name          = "should return `[String \"false\"; Unit; Error]`: with `push \"false\"` on stack [Unit; Error]";
                expected      = [String "false"; Unit; Error];
                value         = "\"false\"";
                initial_state = [Unit; Error]
            };
        ]
        in
        List.iter run_test tests
    
    let test_names () =
        let tests: stack_test list = [
            (* With Empty Stack *)
            {
                name          = "should return `[Name \"x\"]`: with `push x` on empty stack";
                expected      = [Name "x"];
                value         = "x";
                initial_state = []
            };
            {
                name          = {||"should return `[Name "alpha"]`: with `push alpha` on empty stack|};
                expected      = [Name "alpha"];
                value         = "alpha";
                initial_state = []
            };
            {
                name          = {||"should return `[Name "omega3"]`: with `push omega3` on empty stack|};
                expected      = [Name "omega3"];
                value         = "omega3";
                initial_state = []
            };
            {
                name          = {||"should return `[Name "b6"]`: with `push b6` on empty stack|};
                expected      = [Name "b6"];
                value         = "b6";
                initial_state = []
            };
            {
                name          = {||"should return `[Name "undefined_behavior"]`: with `push undefined_behavior` on empty stack|};
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

    let assignment_provided_tests () = ()
end


let suites: unit Alcotest.test list = [
    ("Functions/push", [
        Alcotest.test_case "integers" `Quick TestPush.test_integers;
        Alcotest.test_case "booleans" `Quick TestPush.test_booleans;
        Alcotest.test_case "strings"  `Quick TestPush.test_strings;
        Alcotest.test_case "names"    `Quick TestPush.test_names;
    ]);

    ("Functions/pop", [
        
    ]);
]