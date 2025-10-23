let () =
    let open Alcotest in
    run "Interpreter" (
        Test_functions.suites @
        Test_io.suites        @
        Test_utils.suites     @
        Test_math.suites
    )
