module T = Interpreter.Types

type ('expected_t, 'value_t) test_case = {
    name:      string;
    expected: 'expected_t;
    value:    'value_t;
}


module To_Test = struct
    let resolve_int = Interpreter.Utils.resolve_int
    let is_valid_name = Interpreter.Utils.is_valid_name
end


module TestResolveInt = struct
    let check_resolve_int ~name ~expected ~value =
        Alcotest.(check (Alcotest.option Alcotest.int)) name expected (To_Test.resolve_int value)
    
    
    let run_test { name; expected; value } =
        check_resolve_int ~name ~expected ~value
    
    let test_integers () =
        let tests: (int option, T.value) test_case list = [
            { name = "should return `0`: Integer 0";   expected = (Some 0);    value = (T.Integer   0)  };
            { name = "should return `6`: Integer 6";   expected = (Some 6);    value = (T.Integer   6)  };
            { name = "should return `55`: Integer 55"; expected = (Some 55);   value = (T.Integer  55)  };
            { name = "should return `-1`: Integer -1"; expected = (Some (-1)); value = (T.Integer (-1)) };
        ]
        in
        List.iter run_test tests

    let test_booleans () =
        check_resolve_int ~name:"should return `None`: Boolean true"  ~expected:(None) ~value:(T.Boolean true);
        check_resolve_int ~name:"should return `None`: Boolean false" ~expected:(None) ~value:(T.Boolean false)

    let test_errors () =
        check_resolve_int ~name:"should return `None`: Error" ~expected:(None) ~value:(T.Error)

    let test_unit () =
        check_resolve_int ~name:"should return `None`: Unit" ~expected:(None) ~value:(T.Unit)

    let test_one_char_strings_lowercase () =
        let tests: (int option, T.value) test_case list = [
            { name = "should return `None`: String `a`"; expected = (None); value = (T.String "a") };
            { name = "should return `None`: String `b`"; expected = (None); value = (T.String "b") };
            { name = "should return `None`: String `c`"; expected = (None); value = (T.String "c") };
            { name = "should return `None`: String `d`"; expected = (None); value = (T.String "d") };
            { name = "should return `None`: String `e`"; expected = (None); value = (T.String "e") };
            { name = "should return `None`: String `f`"; expected = (None); value = (T.String "f") };
            { name = "should return `None`: String `g`"; expected = (None); value = (T.String "g") };
            { name = "should return `None`: String `h`"; expected = (None); value = (T.String "h") };
            { name = "should return `None`: String `i`"; expected = (None); value = (T.String "i") };
            { name = "should return `None`: String `j`"; expected = (None); value = (T.String "j") };
            { name = "should return `None`: String `k`"; expected = (None); value = (T.String "k") };
            { name = "should return `None`: String `l`"; expected = (None); value = (T.String "l") };
            { name = "should return `None`: String `m`"; expected = (None); value = (T.String "m") };
            { name = "should return `None`: String `n`"; expected = (None); value = (T.String "n") };
            { name = "should return `None`: String `o`"; expected = (None); value = (T.String "o") };
            { name = "should return `None`: String `p`"; expected = (None); value = (T.String "p") };
            { name = "should return `None`: String `q`"; expected = (None); value = (T.String "q") };
            { name = "should return `None`: String `r`"; expected = (None); value = (T.String "r") };
            { name = "should return `None`: String `s`"; expected = (None); value = (T.String "s") };
            { name = "should return `None`: String `t`"; expected = (None); value = (T.String "t") };
            { name = "should return `None`: String `u`"; expected = (None); value = (T.String "u") };
            { name = "should return `None`: String `v`"; expected = (None); value = (T.String "v") };
            { name = "should return `None`: String `w`"; expected = (None); value = (T.String "w") };
            { name = "should return `None`: String `x`"; expected = (None); value = (T.String "x") };
            { name = "should return `None`: String `y`"; expected = (None); value = (T.String "y") };
            { name = "should return `None`: String `z`"; expected = (None); value = (T.String "z") };
        ]
        in
        List.iter run_test tests

    let test_one_char_strings_uppercase () =
        let tests: (int option, T.value) test_case list = [
            { name = "should return `None`: String `A`"; expected = (None); value = (T.String "A") };
            { name = "should return `None`: String `B`"; expected = (None); value = (T.String "B") };
            { name = "should return `None`: String `C`"; expected = (None); value = (T.String "C") };
            { name = "should return `None`: String `D`"; expected = (None); value = (T.String "D") };
            { name = "should return `None`: String `E`"; expected = (None); value = (T.String "E") };
            { name = "should return `None`: String `F`"; expected = (None); value = (T.String "F") };
            { name = "should return `None`: String `G`"; expected = (None); value = (T.String "G") };
            { name = "should return `None`: String `H`"; expected = (None); value = (T.String "H") };
            { name = "should return `None`: String `I`"; expected = (None); value = (T.String "I") };
            { name = "should return `None`: String `J`"; expected = (None); value = (T.String "J") };
            { name = "should return `None`: String `K`"; expected = (None); value = (T.String "K") };
            { name = "should return `None`: String `L`"; expected = (None); value = (T.String "L") };
            { name = "should return `None`: String `M`"; expected = (None); value = (T.String "M") };
            { name = "should return `None`: String `N`"; expected = (None); value = (T.String "N") };
            { name = "should return `None`: String `O`"; expected = (None); value = (T.String "O") };
            { name = "should return `None`: String `P`"; expected = (None); value = (T.String "P") };
            { name = "should return `None`: String `Q`"; expected = (None); value = (T.String "Q") };
            { name = "should return `None`: String `R`"; expected = (None); value = (T.String "R") };
            { name = "should return `None`: String `S`"; expected = (None); value = (T.String "S") };
            { name = "should return `None`: String `T`"; expected = (None); value = (T.String "T") };
            { name = "should return `None`: String `U`"; expected = (None); value = (T.String "U") };
            { name = "should return `None`: String `V`"; expected = (None); value = (T.String "V") };
            { name = "should return `None`: String `W`"; expected = (None); value = (T.String "W") };
            { name = "should return `None`: String `X`"; expected = (None); value = (T.String "X") };
            { name = "should return `None`: String `Y`"; expected = (None); value = (T.String "Y") };
            { name = "should return `None`: String `Z`"; expected = (None); value = (T.String "Z") };
        ]
        in
        List.iter run_test tests
end


module TestIsValidName = struct
    let check_valid_name ~name ~expected ~value =
        Alcotest.(check bool) name expected (To_Test.is_valid_name value)

    let run_test { name; expected; value } =
        check_valid_name ~name ~expected ~value

    let test_one_letter_lowercase_name () =
        let tests: (bool, string) test_case list = [
            { name = "should return `true`: a"; expected = true; value = "a" };
            { name = "should return `true`: b"; expected = true; value = "b" };
            { name = "should return `true`: c"; expected = true; value = "c" };
            { name = "should return `true`: d"; expected = true; value = "d" };
            { name = "should return `true`: e"; expected = true; value = "e" };
            { name = "should return `true`: f"; expected = true; value = "f" };
            { name = "should return `true`: g"; expected = true; value = "g" };
            { name = "should return `true`: h"; expected = true; value = "h" };
            { name = "should return `true`: i"; expected = true; value = "i" };
            { name = "should return `true`: j"; expected = true; value = "j" };
            { name = "should return `true`: k"; expected = true; value = "k" };
            { name = "should return `true`: l"; expected = true; value = "l" };
            { name = "should return `true`: m"; expected = true; value = "m" };
            { name = "should return `true`: n"; expected = true; value = "n" };
            { name = "should return `true`: o"; expected = true; value = "o" };
            { name = "should return `true`: p"; expected = true; value = "p" };
            { name = "should return `true`: q"; expected = true; value = "q" };
            { name = "should return `true`: r"; expected = true; value = "r" };
            { name = "should return `true`: s"; expected = true; value = "s" };
            { name = "should return `true`: t"; expected = true; value = "t" };
            { name = "should return `true`: u"; expected = true; value = "u" };
            { name = "should return `true`: v"; expected = true; value = "v" };
            { name = "should return `true`: w"; expected = true; value = "w" };
            { name = "should return `true`: x"; expected = true; value = "x" };
            { name = "should return `true`: y"; expected = true; value = "y" };
            { name = "should return `true`: z"; expected = true; value = "z" };
        ]
        in
        List.iter run_test tests

    let test_one_letter_uppercase_name () =
        let tests: (bool, string) test_case list = [
            { name = "should return `true`: A"; expected = true; value = "A" };
            { name = "should return `true`: B"; expected = true; value = "B" };
            { name = "should return `true`: C"; expected = true; value = "C" };
            { name = "should return `true`: D"; expected = true; value = "D" };
            { name = "should return `true`: E"; expected = true; value = "E" };
            { name = "should return `true`: F"; expected = true; value = "F" };
            { name = "should return `true`: G"; expected = true; value = "G" };
            { name = "should return `true`: H"; expected = true; value = "H" };
            { name = "should return `true`: I"; expected = true; value = "I" };
            { name = "should return `true`: J"; expected = true; value = "J" };
            { name = "should return `true`: K"; expected = true; value = "K" };
            { name = "should return `true`: L"; expected = true; value = "L" };
            { name = "should return `true`: M"; expected = true; value = "M" };
            { name = "should return `true`: N"; expected = true; value = "N" };
            { name = "should return `true`: O"; expected = true; value = "O" };
            { name = "should return `true`: P"; expected = true; value = "P" };
            { name = "should return `true`: Q"; expected = true; value = "Q" };
            { name = "should return `true`: R"; expected = true; value = "R" };
            { name = "should return `true`: S"; expected = true; value = "S" };
            { name = "should return `true`: T"; expected = true; value = "T" };
            { name = "should return `true`: U"; expected = true; value = "U" };
            { name = "should return `true`: V"; expected = true; value = "V" };
            { name = "should return `true`: W"; expected = true; value = "W" };
            { name = "should return `true`: X"; expected = true; value = "X" };
            { name = "should return `true`: Y"; expected = true; value = "Y" };
            { name = "should return `true`: Z"; expected = true; value = "Z" };
        ]
        in
        List.iter run_test tests

    let test_two_letter_lowercase_name () =
        let tests: (bool, string) test_case list = [
            { name = "should return `true`: ea"; expected = true; value = "ea" };
            { name = "should return `true`: fb"; expected = true; value = "fb" };
            { name = "should return `true`: gc"; expected = true; value = "gc" };
            { name = "should return `true`: hd"; expected = true; value = "hd" };
            { name = "should return `true`: ie"; expected = true; value = "ie" };
            { name = "should return `true`: jf"; expected = true; value = "jf" };
            { name = "should return `true`: kg"; expected = true; value = "kg" };
            { name = "should return `true`: lh"; expected = true; value = "lh" };
            { name = "should return `true`: mi"; expected = true; value = "mi" };
            { name = "should return `true`: nj"; expected = true; value = "nj" };
            { name = "should return `true`: ok"; expected = true; value = "ok" };
            { name = "should return `true`: pl"; expected = true; value = "pl" };
            { name = "should return `true`: qm"; expected = true; value = "qm" };
            { name = "should return `true`: rn"; expected = true; value = "rn" };
            { name = "should return `true`: so"; expected = true; value = "so" };
            { name = "should return `true`: tp"; expected = true; value = "tp" };
            { name = "should return `true`: uq"; expected = true; value = "uq" };
            { name = "should return `true`: vr"; expected = true; value = "vr" };
            { name = "should return `true`: ws"; expected = true; value = "ws" };
            { name = "should return `true`: xt"; expected = true; value = "xt" };
            { name = "should return `true`: yu"; expected = true; value = "yu" };
            { name = "should return `true`: zv"; expected = true; value = "zv" };
            { name = "should return `true`: iw"; expected = true; value = "iw" };
            { name = "should return `true`: jx"; expected = true; value = "jx" };
            { name = "should return `true`: ky"; expected = true; value = "ky" };
            { name = "should return `true`: lz"; expected = true; value = "lz" };
        ]
        in
        List.iter run_test tests

    let test_two_letter_uppercase_name () =
        let tests: (bool, string) test_case list = [
            { name = "should return `true`: EA"; expected = true; value = "EA" };
            { name = "should return `true`: FB"; expected = true; value = "FB" };
            { name = "should return `true`: GC"; expected = true; value = "GC" };
            { name = "should return `true`: HD"; expected = true; value = "HD" };
            { name = "should return `true`: IE"; expected = true; value = "IE" };
            { name = "should return `true`: JF"; expected = true; value = "JF" };
            { name = "should return `true`: KG"; expected = true; value = "KG" };
            { name = "should return `true`: LH"; expected = true; value = "LH" };
            { name = "should return `true`: MI"; expected = true; value = "MI" };
            { name = "should return `true`: NJ"; expected = true; value = "NJ" };
            { name = "should return `true`: OK"; expected = true; value = "OK" };
            { name = "should return `true`: PL"; expected = true; value = "PL" };
            { name = "should return `true`: QM"; expected = true; value = "QM" };
            { name = "should return `true`: RN"; expected = true; value = "RN" };
            { name = "should return `true`: SO"; expected = true; value = "SO" };
            { name = "should return `true`: TP"; expected = true; value = "TP" };
            { name = "should return `true`: UQ"; expected = true; value = "UQ" };
            { name = "should return `true`: VR"; expected = true; value = "VR" };
            { name = "should return `true`: WS"; expected = true; value = "WS" };
            { name = "should return `true`: XT"; expected = true; value = "XT" };
            { name = "should return `true`: YU"; expected = true; value = "YU" };
            { name = "should return `true`: ZV"; expected = true; value = "ZV" };
            { name = "should return `true`: IW"; expected = true; value = "IW" };
            { name = "should return `true`: JX"; expected = true; value = "JX" };
            { name = "should return `true`: KY"; expected = true; value = "KY" };
            { name = "should return `true`: LZ"; expected = true; value = "LZ" };
        ]
        in
        List.iter run_test tests

    let test_invalid_one_character () =
        let tests: (bool, string) test_case list = [
            { name = "should return false: ` `"; expected = false; value = " " };
            { name = "should return false: `_`"; expected = false; value = "_" };
            { name = "should return false: `0`"; expected = false; value = "0" };
            { name = "should return false: `1`"; expected = false; value = "1" };
            { name = "should return false: `2`"; expected = false; value = "2" };
            { name = "should return false: `3`"; expected = false; value = "3" };
            { name = "should return false: `4`"; expected = false; value = "4" };
            { name = "should return false: `5`"; expected = false; value = "5" };
            { name = "should return false: `6`"; expected = false; value = "6" };
            { name = "should return false: `7`"; expected = false; value = "7" };
            { name = "should return false: `8`"; expected = false; value = "8" };
            { name = "should return false: `9`"; expected = false; value = "9" };
            { name = "should return false: `[`"; expected = false; value = "[" };
            { name = "should return false: `]`"; expected = false; value = "]" };
            { name = "should return false: `-`"; expected = false; value = "-" };
            { name = "should return false: `+`"; expected = false; value = "+" };
            { name = "should return false: `-`"; expected = false; value = "-" };
        ]
        in
        List.iter run_test tests
end



let suites: unit Alcotest.test list = [
    ("Utils/resolve_int", [
        Alcotest.test_case "integers"                     `Quick TestResolveInt.test_integers;
        Alcotest.test_case "booleans"                     `Quick TestResolveInt.test_booleans;
        Alcotest.test_case "errors"                       `Quick TestResolveInt.test_errors;
        Alcotest.test_case "unit"                         `Quick TestResolveInt.test_unit;
        Alcotest.test_case "one char strings (lowercase)" `Quick TestResolveInt.test_one_char_strings_lowercase;
        Alcotest.test_case "one char strings (uppercase)" `Quick TestResolveInt.test_one_char_strings_uppercase;
    ]);

    ("Utils/is_valid_name", [
        Alcotest.test_case "one letter name (lowercase)" `Quick TestIsValidName.test_one_letter_lowercase_name;
        Alcotest.test_case "one letter name (uppercase)" `Quick TestIsValidName.test_one_letter_uppercase_name;
        Alcotest.test_case "two letter name (lowercase)" `Quick TestIsValidName.test_two_letter_lowercase_name;
        Alcotest.test_case "two letter name (uppercase)" `Quick TestIsValidName.test_two_letter_uppercase_name;
        Alcotest.test_case "invalid (one character)"     `Quick TestIsValidName.test_invalid_one_character;
    ])
]
