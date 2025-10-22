module T = Interpreter.Types

type ('expected_t, 'value_t) test_case = {
    name:      string;
    expected: 'expected_t;
    value:    'value_t;
}


module To_Test = struct
    let value_to_string = Interpreter.Utils.value_to_string
    let stack_to_string = Interpreter.Utils.stack_to_string
    let resolve_int = Interpreter.Utils.resolve_int
    let is_valid_name = Interpreter.Utils.is_valid_name
end


module TestPPValue = struct
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


module TestPPStack = struct
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
    ("Utils/value_to_string", [
        Alcotest.test_case "integers"                     `Quick TestPPValue.test_integers;
        Alcotest.test_case "booleans"                     `Quick TestPPValue.test_booleans;
        Alcotest.test_case "errors"                       `Quick TestPPValue.test_errors;
        Alcotest.test_case "unit"                         `Quick TestPPValue.test_unit;
        Alcotest.test_case "one char strings (lowercase)" `Quick TestPPValue.test_one_char_strings_lowercase;
        Alcotest.test_case "one char strings (uppercase)" `Quick TestPPValue.test_one_char_strings_uppercase;
        Alcotest.test_case "pangram (lowercase)"          `Quick TestPPValue.test_pangram_lowercase;
        Alcotest.test_case "pangram (uppercase)"          `Quick TestPPValue.test_pangram_uppercase;
        Alcotest.test_case "lorem ipsum (random case)"    `Quick TestPPValue.test_lorem_ipsum_random_case;
    ]);

    ("Utils/stack_to_string", [
        Alcotest.test_case "empty stack"       `Quick TestPPStack.test_empty_stack;
        Alcotest.test_case "integers"          `Quick TestPPStack.test_integers;
        Alcotest.test_case "booleans"          `Quick TestPPStack.test_booleans;
        Alcotest.test_case "strings"           `Quick TestPPStack.test_strings;
        Alcotest.test_case "unit"              `Quick TestPPStack.test_unit;
        Alcotest.test_case "errors"            `Quick TestPPStack.test_errors;
        Alcotest.test_case "type combinations" `Quick TestPPStack.test_type_combinations;
    ]);

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
