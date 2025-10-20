let () =
    let open Alcotest in
    run "Interpreter" (
        Test_utils.suites @
        Test_math.suites
    )
    