module To_Test = struct
    let pp_value = Interpreter.Utils.pp_value
    let resolve_int = Interpreter.Utils.resolve_int
end


module TestPPValue = struct
    module T = Interpreter.Types

    let check_pp_value name expected value =
        Alcotest.(check string) name expected (To_Test.pp_value value)

    let test_integers () =
        check_pp_value "should return   '0': Integer   0"   "0" (T.Integer    0);
        check_pp_value "should return   '1': Integer   1"   "1" (T.Integer    1);
        check_pp_value "should return '100': Integer 100" "100" (T.Integer  100);
        check_pp_value "should return  '-9': Integer  -9"  "-9" (T.Integer (-9));
        check_pp_value "should return   '0': Integer  -0"   "0" (T.Integer (-0))
end


module TestResolveInt = struct
    module T = Interpreter.Types

    let check_resolve_int name expected value =
        Alcotest.(check (Alcotest.option Alcotest.int)) name expected (To_Test.resolve_int value)
    
    let test_integers () =
        check_resolve_int "should return 0: Integer 0" (Some 0) (T.Integer 0);
        check_resolve_int "should return 6: Integer 6" (Some 6) (T.Integer 6);
        check_resolve_int "should return 55: Integer 55" (Some 55) (T.Integer 55);
        check_resolve_int "should return -1: Integer -1" (Some (-1)) (T.Integer (-1))
end



let suites: unit Alcotest.test list = [
    ("Utils/pp_value", [
        Alcotest.test_case "integers" `Quick TestPPValue.test_integers
    ]);

    ("Utils/resolve_int", [
        Alcotest.test_case "integers" `Quick TestResolveInt.test_integers
    ])
]
