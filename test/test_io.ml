open Test_common


module To_Test = struct
    let value_to_string = Interpreter.Io.value_to_string
    let pp_value = Interpreter.Io.pp_value
    let stack_to_string = Interpreter.Io.stack_to_string
end

module TestValueToString = struct
    let check_value_to_string ~name ~expected ~value =
        Alcotest.(check string) name expected (To_Test.value_to_string value)

    let run_test { name; expected; value } =
        check_value_to_string ~name ~expected ~value

    let test_integers () =
        let tests: (string, T.value) test_case list = [
            { name = "should return `0`: Integer 0";     expected = "0";   value = (T.Integer    0)};
            { name = "should return `1`: Integer 1";     expected = "1";   value = (T.Integer    1)};
            { name = "should return `100`: Integer 100"; expected = "100"; value = (T.Integer  100)};
            { name = "should return `-9`: Integer -9";   expected = "-9";  value = (T.Integer (-9))};
            { name = "should return `0`: Integer -0";    expected = "0";   value = (T.Integer (-0))};
        ]
        in
        List.iter run_test tests
    
    let test_booleans () =
        check_value_to_string ~name:"should return `:true:`: Boolean true"  ~expected:":true:"  ~value:(T.Boolean true);
        check_value_to_string ~name:"should return `:false:`: Boolean true" ~expected:":false:" ~value:(T.Boolean false)

    let test_errors () =
        check_value_to_string ~name:"should return `:error:`: Error" ~expected:":error:" ~value:(T.Error)
    
    let test_unit () =
        check_value_to_string ~name:"should return `:unit:`: Unit" ~expected:":unit:" ~value:(T.Unit)
    
    let test_one_char_strings_lowercase () =
        let tests: (string, T.value) test_case list = [
            { name = "should return `a`: String `a`"; expected = "a"; value = (T.String "a") };
            { name = "should return `b`: String `b`"; expected = "b"; value = (T.String "b") };
            { name = "should return `c`: String `c`"; expected = "c"; value = (T.String "c") };
            { name = "should return `d`: String `d`"; expected = "d"; value = (T.String "d") };
            { name = "should return `e`: String `e`"; expected = "e"; value = (T.String "e") };
            { name = "should return `f`: String `f`"; expected = "f"; value = (T.String "f") };
            { name = "should return `g`: String `g`"; expected = "g"; value = (T.String "g") };
            { name = "should return `h`: String `h`"; expected = "h"; value = (T.String "h") };
            { name = "should return `i`: String `i`"; expected = "i"; value = (T.String "i") };
            { name = "should return `j`: String `j`"; expected = "j"; value = (T.String "j") };
            { name = "should return `k`: String `k`"; expected = "k"; value = (T.String "k") };
            { name = "should return `l`: String `l`"; expected = "l"; value = (T.String "l") };
            { name = "should return `m`: String `m`"; expected = "m"; value = (T.String "m") };
            { name = "should return `n`: String `n`"; expected = "n"; value = (T.String "n") };
            { name = "should return `o`: String `o`"; expected = "o"; value = (T.String "o") };
            { name = "should return `p`: String `p`"; expected = "p"; value = (T.String "p") };
            { name = "should return `q`: String `q`"; expected = "q"; value = (T.String "q") };
            { name = "should return `r`: String `r`"; expected = "r"; value = (T.String "r") };
            { name = "should return `s`: String `s`"; expected = "s"; value = (T.String "s") };
            { name = "should return `t`: String `t`"; expected = "t"; value = (T.String "t") };
            { name = "should return `u`: String `u`"; expected = "u"; value = (T.String "u") };
            { name = "should return `v`: String `v`"; expected = "v"; value = (T.String "v") };
            { name = "should return `w`: String `w`"; expected = "w"; value = (T.String "w") };
            { name = "should return `x`: String `x`"; expected = "x"; value = (T.String "x") };
            { name = "should return `y`: String `y`"; expected = "y"; value = (T.String "y") };
            { name = "should return `z`: String `z`"; expected = "z"; value = (T.String "z") };
        ]
        in
        List.iter run_test tests
    
    let test_one_char_strings_uppercase () =
        let tests: (string, T.value) test_case list = [
            { name = "should return `A`: String `A`"; expected = "A"; value = (T.String "A") };
            { name = "should return `B`: String `B`"; expected = "B"; value = (T.String "B") };
            { name = "should return `C`: String `C`"; expected = "C"; value = (T.String "C") };
            { name = "should return `D`: String `D`"; expected = "D"; value = (T.String "D") };
            { name = "should return `E`: String `E`"; expected = "E"; value = (T.String "E") };
            { name = "should return `F`: String `F`"; expected = "F"; value = (T.String "F") };
            { name = "should return `G`: String `G`"; expected = "G"; value = (T.String "G") };
            { name = "should return `H`: String `H`"; expected = "H"; value = (T.String "H") };
            { name = "should return `I`: String `I`"; expected = "I"; value = (T.String "I") };
            { name = "should return `J`: String `J`"; expected = "J"; value = (T.String "J") };
            { name = "should return `K`: String `K`"; expected = "K"; value = (T.String "K") };
            { name = "should return `L`: String `L`"; expected = "L"; value = (T.String "L") };
            { name = "should return `M`: String `M`"; expected = "M"; value = (T.String "M") };
            { name = "should return `N`: String `N`"; expected = "N"; value = (T.String "N") };
            { name = "should return `O`: String `O`"; expected = "O"; value = (T.String "O") };
            { name = "should return `P`: String `P`"; expected = "P"; value = (T.String "P") };
            { name = "should return `Q`: String `Q`"; expected = "Q"; value = (T.String "Q") };
            { name = "should return `R`: String `R`"; expected = "R"; value = (T.String "R") };
            { name = "should return `S`: String `S`"; expected = "S"; value = (T.String "S") };
            { name = "should return `T`: String `T`"; expected = "T"; value = (T.String "T") };
            { name = "should return `U`: String `U`"; expected = "U"; value = (T.String "U") };
            { name = "should return `V`: String `V`"; expected = "V"; value = (T.String "V") };
            { name = "should return `W`: String `W`"; expected = "W"; value = (T.String "W") };
            { name = "should return `X`: String `X`"; expected = "X"; value = (T.String "X") };
            { name = "should return `Y`: String `Y`"; expected = "Y"; value = (T.String "Y") };
            { name = "should return `Z`: String `Z`"; expected = "Z"; value = (T.String "Z") };
        ]
        in
        List.iter run_test tests

    let test_pangram_lowercase () =
        let tests: (string, T.value) test_case list = [
            {
                name     = "should return `the quick brown fox jumps over the lazy dog`: String `the quick brown fox jumps over the lazy dog`";
                expected = "the quick brown fox jumps over the lazy dog";
                value    = (T.String "the quick brown fox jumps over the lazy dog");
            };
            {
                name     = "should return `quick nymph bugs vex fjord waltz`: String `quick nymph bugs vex fjord waltz`";
                expected =  "quick nymph bugs vex fjord waltz";
                value    = (T.String "quick nymph bugs vex fjord waltz");
            };
            {
                name     = "should return `sphinx of black quartz, judge my vow`: String `sphinx of black quartz, judge my vow`";
                expected =  "sphinx of black quartz, judge my vow";
                value    = (T.String "sphinx of black quartz, judge my vow");
            };
        ]
        in
        List.iter run_test tests

    let test_pangram_uppercase () =
        let tests: (string, T.value) test_case list = [
            {
                name = "should return `THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG`: String `THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG`";
                expected = "THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG";
                value = (T.String "THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG");
            };
            {
                name = "should return `QUICK NYMPH BUGS VEX FJORD WALTZ`: String `QUICK NYMPH BUGS VEX FJORD WALTZ`";
                expected = "QUICK NYMPH BUGS VEX FJORD WALTZ";
                value = (T.String "QUICK NYMPH BUGS VEX FJORD WALTZ");
            };
            {
                name = "should return `SPHINX OF BLACK QUARTZ, JUDGE MY VOW`: String `SPHINX OF BLACK QUARTZ, JUDGE MY VOW`";
                expected = "SPHINX OF BLACK QUARTZ, JUDGE MY VOW";
                value = (T.String "SPHINX OF BLACK QUARTZ, JUDGE MY VOW");
            };
        ]
        in
        List.iter run_test tests
    
    let test_lorem_ipsum_random_case () =
        let tests: (string, T.value) test_case list = [
            {
                name = (
                    "should return `LoReM iPsum DoLoR sit aMET, COnSECTETuR AdIpIScING ELIt. nAm VIVErRa matTis RIsus TRISTiqUE MAXiMUS`:" ^
                    "String `LoReM iPsum DoLoR sit aMET, COnSECTETuR AdIpIScING ELIt. nAm VIVErRa matTis RIsus TRISTiqUE MAXiMUS`"
                );
                expected = "LoReM iPsum DoLoR sit aMET, COnSECTETuR AdIpIScING ELIt. nAm VIVErRa matTis RIsus TRISTiqUE MAXiMUS";
                value    = (T.String "LoReM iPsum DoLoR sit aMET, COnSECTETuR AdIpIScING ELIt. nAm VIVErRa matTis RIsus TRISTiqUE MAXiMUS");
            };
            {
                name = (
                    "should return `seD A mASSA noN urNa FaUCibUs BLANDiT. duIS INTERDUM POrtTIToR URNa Ac cuRsuS. sED sED LoBortIS lorem`: " ^
                    "String `seD A mASSA noN urNa FaUCibUs BLANDiT. duIS INTERDUM POrtTIToR URNa Ac cuRsuS. sED sED LoBortIS lorem`"
                );
                expected = "seD A mASSA noN urNa FaUCibUs BLANDiT. duIS INTERDUM POrtTIToR URNa Ac cuRsuS. sED sED LoBortIS lorem";
                value    = (T.String "seD A mASSA noN urNa FaUCibUs BLANDiT. duIS INTERDUM POrtTIToR URNa Ac cuRsuS. sED sED LoBortIS lorem");
            };
        ]
        in
        List.iter run_test tests
end


module TestStackToString = struct
    type stack_test = {
        name:      string;
        expected:  string;
        value:     T.stack;
        separator: string option;
    }

    let check_stack_to_string ~name ~expected ~value ?separator () =
        Alcotest.(check string) name expected (To_Test.stack_to_string ?separator value)
    
    let run_test { name; expected; value; separator } =
        check_stack_to_string ~name ~expected ~value ?separator ()

    let test_empty_stack () =
        let tests: stack_test list = [
            {
                name      = "should return empty string: Stack []";
                expected  = "";
                value     = ([]);
                separator = None
            };
            {
                name      = "should return empty string: Stack []";
                expected  = "";
                value     = ([]);
                separator = Some "\n"
            };
            {
                name      = "should return empty string: Stack []";
                expected  = "";
                value     = ([]);
                separator = Some ";"
            };
            {
                name      = "should return empty string: Stack []";
                expected  = "";
                value     = ([]);
                separator = Some "--"
            };
            {
                name      = "should return empty string: Stack []";
                expected  = "";
                value     = ([]);
                separator = Some "\t"
            };
        ]
        in
        List.iter run_test tests

    let test_integers () =
        let tests: stack_test list = [
            (* Single Element Stacks *)
            {
                name      = "should return `1`: Stack [Integer 1]";
                expected  = "1";
                value     = ([T.Integer 1]);
                separator = None
            };
            {
                name      = "should return `77`: Stack [Integer 77] | separator=`,`";
                expected  = "77";
                value     = ([T.Integer 77]);
                separator = Some ","
            };
            {
                name      = "should return `-42`: Stack [Integer -42] | separator=`:`";
                expected  = "-42";
                value     = ([T.Integer (-42)]);
                separator = Some ":"
            };
            {
                name      = "should return `0`: Stack [Integer 0] | separator=`-`";
                expected  = "0";
                value     = ([T.Integer (0)]);
                separator = Some "-"
            };
            {
                name      = "should return `0`: Stack [Integer -0] | separator=`;`";
                expected  = "0";
                value     = ([T.Integer (-0)]);
                separator = Some ";"
            };
            {
                name      = "should return `1001`: Stack [Integer 1001] | separator=`;;`";
                expected  = "1001";
                value     = ([T.Integer (1_001)]);
                separator = Some ";;"
            };
            {
                name      = "should return `-99`: Stack [Integer -99] | separator=`\\n`(newline)";
                expected  = "-99";
                value     = ([T.Integer (-99)]);
                separator = Some "\n"
            };

            (* Double Element Stack *)
            {
                name      = "should return `0 0`: Stack [Integer 0; Integer 0]";
                expected  = "0 0";
                value     = ([T.Integer 0; T.Integer 0]);
                separator = None
            };
            {
                name      = "should return `0 0`: Stack [Integer -0; Integer -0] | separator=`;`";
                expected  = "0;0";
                value     = ([T.Integer (-0); T.Integer (-0)]);
                separator = Some ";"
            };
            {
                name      = "should return `959595`: Stack [Integer 959; Integer 595] | separator=``";
                expected  = "959595";
                value     = ([T.Integer 959; T.Integer 595]);
                separator = Some ""
            };
            {
                name      = "should return `42--42`: Stack [Integer 42; Integer 42] | separator=`--`";
                expected  = "42--42";
                value     = ([T.Integer 42; T.Integer 42]);
                separator = Some "--"
            };
        ]
        in
        List.iter run_test tests
        
    let test_booleans () =
        let tests: stack_test list = [
            {
                name      = "should return `:true:`: Stack [Boolean true]";
                expected  = ":true:";
                value     = ([T.Boolean true]);
                separator = None
            };
            {
                name      = "should return `:false:`: Stack [Boolean false] | separator=`_-`";
                expected  = ":false:";
                value     = ([T.Boolean false]);
                separator = Some "_-"
            };
            {
                name      = "should return `:false:_-_:true:`: Stack [Boolean false; Boolean true] | separator=`_-_`";
                expected  = ":false:_-_:true:";
                value     = ([T.Boolean false; T.Boolean true]);
                separator = Some "_-_"
            };
            {
                name      = "should return `:true: :true:`: Stack [Boolean true; Boolean true] | separator=` `";
                expected  = ":true: :true:";
                value     = ([T.Boolean true; T.Boolean true]);
                separator = Some " "
            };
            {
                name      = "should return `:false: :false:`: Stack [Boolean false; Boolean false]";
                expected  = ":false: :false:";
                value     = ([T.Boolean false; T.Boolean false]);
                separator = None
            };
        ]
        in
        List.iter run_test tests
    
    let test_strings () =
        let tests: stack_test list = [
            {
                name      = "should return empty string: Stack [String ``]";
                expected  = "";
                value     = ([T.String ""]);
                separator = None
            };
            {
                name      = "should return `Hello World!`: Stack [String `Hello`; String `World!`]";
                expected  = "Hello World!";
                value     = ([T.String "Hello"; T.String "World!"]);
                separator = None
            };
            {
                name      = "should return `Love=Ocaml`: Stack [String `Love`; `Ocaml`] | separator=`=`";
                expected  = "Love=Ocaml";
                value     = ([T.String "Love"; T.String "Ocaml"]);
                separator = Some "="
            };
            {
                name      = "should return `Lorem ipsum dolor`: Stack [String `Lorem`; String `ipsum`; String `dolor`] | separator=` `";
                expected  = "Lorem ipsum dolor";
                value     = ([T.String "Lorem"; T.String "ipsum"; T.String "dolor"]);
                separator = Some " "
            };
        ]
        in
        List.iter run_test tests
    
    let test_unit () =
        let tests: stack_test list = [
            {
                name      = "should return `:unit:`: Stack [Unit]";
                expected  = ":unit:";
                value     = ([T.Unit]);
                separator = None
            };
            {
                name      = "should return empty `:unit:^^:unit:^^:unit:`: Stack [Unit; Unit; Unit] | separator=`^^`";
                expected  = ":unit:^^:unit:^^:unit:";
                value     = ([T.Unit; T.Unit; T.Unit]);
                separator = Some "^^"
            };
        ]
        in
        List.iter run_test tests

    let test_errors () =
        let tests: stack_test list = [
            {
                name      = "should return `:error`: Stack [Error]";
                expected  = ":error:";
                value     = ([T.Error]);
                separator = None
            };
            {
                name      = "should return `:error:---:error:---:error:`: Stack [Error; Error; Error] | separator=`---`";
                expected  = ":error:---:error:---:error:";
                value     = ([T.Error; T.Error; T.Error]);
                separator = Some "---"
            };
            {
                name      = "should return `:error:___:error:`: Stack [Error; Error] | separator=`___`";
                expected  = ":error:";
                value     = ([T.Error]);
                separator = Some "___"
            };
        ]
        in
        List.iter run_test tests

    let test_type_combinations () =
        let tests: stack_test list = [
            {
                name      = "should return `:error:;-;Hello;-;:error:`: Stack [Error; String `Hello`; Error] | separator=`;-;`";
                expected  = ":error:;-;Hello;-;:error:";
                value     = ([T.Error; T.String "Hello"; T.Error]);
                separator = Some ";-;"
            };
            {
                name      = "should return `:unit: Hello :unit: Message :true: :error:`: Stack [Unit; String `Hello`; Unit; String `Message`; Boolean true; Error]";
                expected  = ":unit: Hello :unit: Message :true: :error:";
                value     = ([T.Unit; T.String "Hello"; T.Unit; T.String "Message"; T.Boolean true; T.Error]);
                separator = None
            };
            {
                name      = "should return `:unit:\\t:unit:\\tMy\\tMessage\\t:unit:\\t:unit:`: Stack [Unit; Unit; String `My`; String `Message`; Unit; Unit] | separator=`\t`(tab)";
                expected  = ":unit:\t:unit:\tMy\tMessage\t:unit:\t:unit:";
                value     = ([T.Unit; T.Unit; T.String "My"; T.String "Message"; T.Unit; T.Unit]);
                separator = Some "\t"
            };
            {
                name      = "should return `Value of X is 7 :unit:`: Stack [String `Value`; String `of`; String `X`; String `is`; Integer 7; Unit]";
                expected  = "Value of X is 7 :unit:";
                value     = ([T.String "Value"; T.String "of"; T.String "X"; T.String "is"; T.Integer 7; T.Unit]);
                separator = None
            };
            {
                name      = "should return `Developed in OCaml 5.3.0`: Stack [String `Developed in Ocaml 5`; Integer 3; Integer 0] | separator=`.`";
                expected  = "Developed in Ocaml 5.3.0";
                value     = ([T.String "Developed in Ocaml 5"; T.Integer 3; T.Integer 0]);
                separator = Some "."
            };
        ]
        in
        List.iter run_test tests
end


let suites: unit Alcotest.test list = [
    ("IO/value_to_string", [
        Alcotest.test_case "integers"                     `Quick TestValueToString.test_integers;
        Alcotest.test_case "booleans"                     `Quick TestValueToString.test_booleans;
        Alcotest.test_case "errors"                       `Quick TestValueToString.test_errors;
        Alcotest.test_case "unit"                         `Quick TestValueToString.test_unit;
        Alcotest.test_case "one char strings (lowercase)" `Quick TestValueToString.test_one_char_strings_lowercase;
        Alcotest.test_case "one char strings (uppercase)" `Quick TestValueToString.test_one_char_strings_uppercase;
        Alcotest.test_case "pangram (lowercase)"          `Quick TestValueToString.test_pangram_lowercase;
        Alcotest.test_case "pangram (uppercase)"          `Quick TestValueToString.test_pangram_uppercase;
        Alcotest.test_case "lorem ipsum (random case)"    `Quick TestValueToString.test_lorem_ipsum_random_case;
    ]);

    ("IO/stack_to_string", [
        Alcotest.test_case "empty stack"       `Quick TestStackToString.test_empty_stack;
        Alcotest.test_case "integers"          `Quick TestStackToString.test_integers;
        Alcotest.test_case "booleans"          `Quick TestStackToString.test_booleans;
        Alcotest.test_case "strings"           `Quick TestStackToString.test_strings;
        Alcotest.test_case "unit"              `Quick TestStackToString.test_unit;
        Alcotest.test_case "errors"            `Quick TestStackToString.test_errors;
        Alcotest.test_case "type combinations" `Quick TestStackToString.test_type_combinations;
    ]);
]
