module T = Interpreter.Types


module To_Test = struct
    let pp_value = Interpreter.Utils.pp_value
    let resolve_int = Interpreter.Utils.resolve_int
    let is_valid_name = Interpreter.Utils.is_valid_name
end


module TestPPValue = struct
    let check_pp_value name expected value =
        Alcotest.(check string) name expected (To_Test.pp_value value)

    let test_integers () =
        check_pp_value "should return   `0`: Integer   0"   "0" (T.Integer    0);
        check_pp_value "should return   `1`: Integer   1"   "1" (T.Integer    1);
        check_pp_value "should return `100`: Integer 100" "100" (T.Integer  100);
        check_pp_value "should return  `-9`: Integer  -9"  "-9" (T.Integer (-9));
        check_pp_value "should return   `0`: Integer  -0"   "0" (T.Integer (-0))
    
    let test_booleans () =
        check_pp_value "should return `:true:`: Boolean true"  ":true:"  (T.Boolean true);
        check_pp_value "should return `:false:`: Boolean true" ":false:" (T.Boolean false)

    let test_errors () =
        check_pp_value "should return `:error:`: Error" ":error:" (T.Error)
    
    let test_unit () =
        check_pp_value "should return `:unit:`: Unit" ":unit:" (T.Unit)
    
    let test_one_char_strings_lowercase () =
        check_pp_value "should return `a`: String `a`" "a" (T.String "a");
        check_pp_value "should return `b`: String `b`" "b" (T.String "b");
        check_pp_value "should return `c`: String `c`" "c" (T.String "c");
        check_pp_value "should return `d`: String `d`" "d" (T.String "d");
        check_pp_value "should return `e`: String `e`" "e" (T.String "e");
        check_pp_value "should return `f`: String `f`" "f" (T.String "f");
        check_pp_value "should return `g`: String `g`" "g" (T.String "g");
        check_pp_value "should return `h`: String `h`" "h" (T.String "h");
        check_pp_value "should return `i`: String `i`" "i" (T.String "i");
        check_pp_value "should return `j`: String `j`" "j" (T.String "j");
        check_pp_value "should return `k`: String `k`" "k" (T.String "k");
        check_pp_value "should return `l`: String `l`" "l" (T.String "l");
        check_pp_value "should return `m`: String `m`" "m" (T.String "m");
        check_pp_value "should return `n`: String `n`" "n" (T.String "n");
        check_pp_value "should return `o`: String `o`" "o" (T.String "o");
        check_pp_value "should return `p`: String `p`" "p" (T.String "p");
        check_pp_value "should return `q`: String `q`" "q" (T.String "q");
        check_pp_value "should return `r`: String `r`" "r" (T.String "r");
        check_pp_value "should return `s`: String `s`" "s" (T.String "s");
        check_pp_value "should return `t`: String `t`" "t" (T.String "t");
        check_pp_value "should return `u`: String `u`" "u" (T.String "u");
        check_pp_value "should return `v`: String `v`" "v" (T.String "v");
        check_pp_value "should return `w`: String `w`" "w" (T.String "w");
        check_pp_value "should return `x`: String `x`" "x" (T.String "x");
        check_pp_value "should return `y`: String `y`" "y" (T.String "y");
        check_pp_value "should return `z`: String `z`" "z" (T.String "z")
    
    let test_one_char_strings_uppercase () =
        check_pp_value "should return `A`: String `A`" "A" (T.String "A");
        check_pp_value "should return `B`: String `B`" "B" (T.String "B");
        check_pp_value "should return `C`: String `C`" "C" (T.String "C");
        check_pp_value "should return `D`: String `D`" "D" (T.String "D");
        check_pp_value "should return `E`: String `E`" "E" (T.String "E");
        check_pp_value "should return `F`: String `F`" "F" (T.String "F");
        check_pp_value "should return `G`: String `G`" "G" (T.String "G");
        check_pp_value "should return `H`: String `H`" "H" (T.String "H");
        check_pp_value "should return `I`: String `I`" "I" (T.String "I");
        check_pp_value "should return `J`: String `J`" "J" (T.String "J");
        check_pp_value "should return `K`: String `K`" "K" (T.String "K");
        check_pp_value "should return `L`: String `L`" "L" (T.String "L");
        check_pp_value "should return `M`: String `M`" "M" (T.String "M");
        check_pp_value "should return `N`: String `N`" "N" (T.String "N");
        check_pp_value "should return `O`: String `O`" "O" (T.String "O");
        check_pp_value "should return `P`: String `P`" "P" (T.String "P");
        check_pp_value "should return `Q`: String `Q`" "Q" (T.String "Q");
        check_pp_value "should return `R`: String `R`" "R" (T.String "R");
        check_pp_value "should return `S`: String `S`" "S" (T.String "S");
        check_pp_value "should return `T`: String `T`" "T" (T.String "T");
        check_pp_value "should return `U`: String `U`" "U" (T.String "U");
        check_pp_value "should return `V`: String `V`" "V" (T.String "V");
        check_pp_value "should return `W`: String `W`" "W" (T.String "W");
        check_pp_value "should return `X`: String `X`" "X" (T.String "X");
        check_pp_value "should return `Y`: String `Y`" "Y" (T.String "Y");
        check_pp_value "should return `Z`: String `Z`" "Z" (T.String "Z")

    let test_pangram_lowercase () =
        check_pp_value
            "should return `the quick brown fox jumps over the lazy dog`: String `the quick brown fox jumps over the lazy dog`"
            "the quick brown fox jumps over the lazy dog"
            (T.String "the quick brown fox jumps over the lazy dog");
        check_pp_value
            "should return `quick nymph bugs vex fjord waltz`: String `quick nymph bugs vex fjord waltz`"
            "quick nymph bugs vex fjord waltz"
            (T.String "quick nymph bugs vex fjord waltz");
        check_pp_value
            "should return `sphinx of black quartz, judge my vow`: String `sphinx of black quartz, judge my vow`"
            "sphinx of black quartz, judge my vow"
            (T.String "sphinx of black quartz, judge my vow")

    let test_pangram_uppercase () =
        check_pp_value
            "should return `the quick brown fox jumps over the lazy dog`: String `the quick brown fox jumps over the lazy dog`"
            "the quick brown fox jumps over the lazy dog"
            (T.String "the quick brown fox jumps over the lazy dog");
        check_pp_value
            "should return `quick nymph bugs vex fjord waltz`: String `quick nymph bugs vex fjord waltz`"
            "quick nymph bugs vex fjord waltz"
            (T.String "quick nymph bugs vex fjord waltz");
        check_pp_value
            "should return `sphinx of black quartz, judge my vow`: String `sphinx of black quartz, judge my vow`"
            "sphinx of black quartz, judge my vow"
            (T.String "sphinx of black quartz, judge my vow")
    
    let test_lorem_ipsum_random_case () =
        check_pp_value
            ("should return `LoReM iPsum DoLoR sit aMET, COnSECTETuR AdIpIScING ELIt. nAm VIVErRa matTis RIsus TRISTiqUE MAXiMUS`:" ^
            "String `LoReM iPsum DoLoR sit aMET, COnSECTETuR AdIpIScING ELIt. nAm VIVErRa matTis RIsus TRISTiqUE MAXiMUS`")
            "LoReM iPsum DoLoR sit aMET, COnSECTETuR AdIpIScING ELIt. nAm VIVErRa matTis RIsus TRISTiqUE MAXiMUS"
            (T.String "LoReM iPsum DoLoR sit aMET, COnSECTETuR AdIpIScING ELIt. nAm VIVErRa matTis RIsus TRISTiqUE MAXiMUS");
        check_pp_value
            ("should return `seD A mASSA noN urNa FaUCibUs BLANDiT. duIS INTERDUM POrtTIToR URNa Ac cuRsuS. sED sED LoBortIS lorem`:" ^
            "String `seD A mASSA noN urNa FaUCibUs BLANDiT. duIS INTERDUM POrtTIToR URNa Ac cuRsuS. sED sED LoBortIS lorem`")
            "seD A mASSA noN urNa FaUCibUs BLANDiT. duIS INTERDUM POrtTIToR URNa Ac cuRsuS. sED sED LoBortIS lorem"
            (T.String "seD A mASSA noN urNa FaUCibUs BLANDiT. duIS INTERDUM POrtTIToR URNa Ac cuRsuS. sED sED LoBortIS lorem");
end


module TestResolveInt = struct
    let check_resolve_int name expected value =
        Alcotest.(check (Alcotest.option Alcotest.int)) name expected (To_Test.resolve_int value)
    
    let test_integers () =
        check_resolve_int "should return  `0`: Integer  0" (Some   0)  (T.Integer   0);
        check_resolve_int "should return  `6`: Integer  6" (Some   6)  (T.Integer   6);
        check_resolve_int "should return `55`: Integer 55" (Some  55)  (T.Integer  55);
        check_resolve_int "should return `-1`: Integer -1" (Some (-1)) (T.Integer (-1))        

    let test_booleans () =
        check_resolve_int "should return `None`: Boolean true"  (None)  (T.Boolean true);
        check_resolve_int "should return `None`: Boolean false" (None)  (T.Boolean false)

    let test_errors () =
        check_resolve_int "should return `None`: Error" (None) (T.Error)

    let test_unit () =
        check_resolve_int "should return `None`: Unit" (None) (T.Unit)

    let test_one_char_strings_lowercase () =
        check_resolve_int "should return `None`: String `a`" (None) (T.String "a");
        check_resolve_int "should return `None`: String `b`" (None) (T.String "b");
        check_resolve_int "should return `None`: String `c`" (None) (T.String "c");
        check_resolve_int "should return `None`: String `d`" (None) (T.String "d");
        check_resolve_int "should return `None`: String `e`" (None) (T.String "e");
        check_resolve_int "should return `None`: String `f`" (None) (T.String "f");
        check_resolve_int "should return `None`: String `g`" (None) (T.String "g");
        check_resolve_int "should return `None`: String `h`" (None) (T.String "h");
        check_resolve_int "should return `None`: String `i`" (None) (T.String "i");
        check_resolve_int "should return `None`: String `j`" (None) (T.String "j");
        check_resolve_int "should return `None`: String `k`" (None) (T.String "k");
        check_resolve_int "should return `None`: String `l`" (None) (T.String "l");
        check_resolve_int "should return `None`: String `m`" (None) (T.String "m");
        check_resolve_int "should return `None`: String `n`" (None) (T.String "n");
        check_resolve_int "should return `None`: String `o`" (None) (T.String "o");
        check_resolve_int "should return `None`: String `p`" (None) (T.String "p");
        check_resolve_int "should return `None`: String `q`" (None) (T.String "q");
        check_resolve_int "should return `None`: String `r`" (None) (T.String "r");
        check_resolve_int "should return `None`: String `s`" (None) (T.String "s");
        check_resolve_int "should return `None`: String `t`" (None) (T.String "t");
        check_resolve_int "should return `None`: String `u`" (None) (T.String "u");
        check_resolve_int "should return `None`: String `v`" (None) (T.String "v");
        check_resolve_int "should return `None`: String `w`" (None) (T.String "w");
        check_resolve_int "should return `None`: String `x`" (None) (T.String "x");
        check_resolve_int "should return `None`: String `y`" (None) (T.String "y");
        check_resolve_int "should return `None`: String `z`" (None) (T.String "z")

    let test_one_char_strings_uppercase () =
        check_resolve_int "should return `None`: String `A`" (None) (T.String "A");
        check_resolve_int "should return `None`: String `B`" (None) (T.String "B");
        check_resolve_int "should return `None`: String `C`" (None) (T.String "C");
        check_resolve_int "should return `None`: String `D`" (None) (T.String "D");
        check_resolve_int "should return `None`: String `E`" (None) (T.String "E");
        check_resolve_int "should return `None`: String `F`" (None) (T.String "F");
        check_resolve_int "should return `None`: String `G`" (None) (T.String "G");
        check_resolve_int "should return `None`: String `H`" (None) (T.String "H");
        check_resolve_int "should return `None`: String `I`" (None) (T.String "I");
        check_resolve_int "should return `None`: String `J`" (None) (T.String "J");
        check_resolve_int "should return `None`: String `K`" (None) (T.String "K");
        check_resolve_int "should return `None`: String `L`" (None) (T.String "L");
        check_resolve_int "should return `None`: String `M`" (None) (T.String "M");
        check_resolve_int "should return `None`: String `N`" (None) (T.String "N");
        check_resolve_int "should return `None`: String `O`" (None) (T.String "O");
        check_resolve_int "should return `None`: String `P`" (None) (T.String "P");
        check_resolve_int "should return `None`: String `Q`" (None) (T.String "Q");
        check_resolve_int "should return `None`: String `R`" (None) (T.String "R");
        check_resolve_int "should return `None`: String `S`" (None) (T.String "S");
        check_resolve_int "should return `None`: String `T`" (None) (T.String "T");
        check_resolve_int "should return `None`: String `U`" (None) (T.String "U");
        check_resolve_int "should return `None`: String `V`" (None) (T.String "V");
        check_resolve_int "should return `None`: String `W`" (None) (T.String "W");
        check_resolve_int "should return `None`: String `X`" (None) (T.String "X");
        check_resolve_int "should return `None`: String `Y`" (None) (T.String "Y");
        check_resolve_int "should return `None`: String `Z`" (None) (T.String "Z")
end


module TestIsValidName = struct
    let check_valid_name name expected value =
        Alcotest.(check bool) name expected (To_Test.is_valid_name value)

    let test_one_letter_lowercase_name () =
        check_valid_name "should return `true`: a" true "a";
        check_valid_name "should return `true`: b" true "b";
        check_valid_name "should return `true`: c" true "c";
        check_valid_name "should return `true`: d" true "d";
        check_valid_name "should return `true`: e" true "e";
        check_valid_name "should return `true`: f" true "f";
        check_valid_name "should return `true`: g" true "g";
        check_valid_name "should return `true`: h" true "h";
        check_valid_name "should return `true`: i" true "i";
        check_valid_name "should return `true`: j" true "j";
        check_valid_name "should return `true`: k" true "k";
        check_valid_name "should return `true`: l" true "l";
        check_valid_name "should return `true`: m" true "m";
        check_valid_name "should return `true`: n" true "n";
        check_valid_name "should return `true`: o" true "o";
        check_valid_name "should return `true`: p" true "p";
        check_valid_name "should return `true`: q" true "q";
        check_valid_name "should return `true`: r" true "r";
        check_valid_name "should return `true`: s" true "s";
        check_valid_name "should return `true`: t" true "t";
        check_valid_name "should return `true`: u" true "u";
        check_valid_name "should return `true`: v" true "v";
        check_valid_name "should return `true`: w" true "w";
        check_valid_name "should return `true`: x" true "x";
        check_valid_name "should return `true`: y" true "y";
        check_valid_name "should return `true`: z" true "z"

    let test_one_letter_uppercase_name () =
        check_valid_name "should return `true`: A" true "A";
        check_valid_name "should return `true`: B" true "B";
        check_valid_name "should return `true`: C" true "C";
        check_valid_name "should return `true`: D" true "D";
        check_valid_name "should return `true`: E" true "E";
        check_valid_name "should return `true`: F" true "F";
        check_valid_name "should return `true`: G" true "G";
        check_valid_name "should return `true`: H" true "H";
        check_valid_name "should return `true`: I" true "I";
        check_valid_name "should return `true`: J" true "J";
        check_valid_name "should return `true`: K" true "K";
        check_valid_name "should return `true`: L" true "L";
        check_valid_name "should return `true`: M" true "M";
        check_valid_name "should return `true`: N" true "N";
        check_valid_name "should return `true`: O" true "O";
        check_valid_name "should return `true`: P" true "P";
        check_valid_name "should return `true`: Q" true "Q";
        check_valid_name "should return `true`: R" true "R";
        check_valid_name "should return `true`: S" true "S";
        check_valid_name "should return `true`: T" true "T";
        check_valid_name "should return `true`: U" true "U";
        check_valid_name "should return `true`: V" true "V";
        check_valid_name "should return `true`: W" true "W";
        check_valid_name "should return `true`: X" true "X";
        check_valid_name "should return `true`: Y" true "Y";
        check_valid_name "should return `true`: Z" true "Z"

    let test_two_letter_lowercase_name () =
        check_valid_name "should return `true`: ea" true "ea";
        check_valid_name "should return `true`: fb" true "fb";
        check_valid_name "should return `true`: gc" true "gc";
        check_valid_name "should return `true`: hd" true "hd";
        check_valid_name "should return `true`: ie" true "ie";
        check_valid_name "should return `true`: jf" true "jf";
        check_valid_name "should return `true`: kg" true "kg";
        check_valid_name "should return `true`: lh" true "lh";
        check_valid_name "should return `true`: mi" true "mi";
        check_valid_name "should return `true`: nj" true "nj";
        check_valid_name "should return `true`: ok" true "ok";
        check_valid_name "should return `true`: pl" true "pl";
        check_valid_name "should return `true`: qm" true "qm";
        check_valid_name "should return `true`: rn" true "rn";
        check_valid_name "should return `true`: so" true "so";
        check_valid_name "should return `true`: tp" true "tp";
        check_valid_name "should return `true`: uq" true "uq";
        check_valid_name "should return `true`: vr" true "vr";
        check_valid_name "should return `true`: ws" true "ws";
        check_valid_name "should return `true`: xt" true "xt";
        check_valid_name "should return `true`: yu" true "yu";
        check_valid_name "should return `true`: zv" true "zv";
        check_valid_name "should return `true`: iw" true "iw";
        check_valid_name "should return `true`: jx" true "jx";
        check_valid_name "should return `true`: ky" true "ky";
        check_valid_name "should return `true`: lz" true "lz"

    let test_two_letter_uppercase_name () =
        check_valid_name "should return `true`: EA" true "EA";
        check_valid_name "should return `true`: FB" true "FB";
        check_valid_name "should return `true`: GC" true "GC";
        check_valid_name "should return `true`: HD" true "HD";
        check_valid_name "should return `true`: IE" true "IE";
        check_valid_name "should return `true`: JF" true "JF";
        check_valid_name "should return `true`: KG" true "KG";
        check_valid_name "should return `true`: LH" true "LH";
        check_valid_name "should return `true`: MI" true "MI";
        check_valid_name "should return `true`: NJ" true "NJ";
        check_valid_name "should return `true`: OK" true "OK";
        check_valid_name "should return `true`: PL" true "PL";
        check_valid_name "should return `true`: QM" true "QM";
        check_valid_name "should return `true`: RN" true "RN";
        check_valid_name "should return `true`: SO" true "SO";
        check_valid_name "should return `true`: TP" true "TP";
        check_valid_name "should return `true`: UQ" true "UQ";
        check_valid_name "should return `true`: VR" true "VR";
        check_valid_name "should return `true`: WS" true "WS";
        check_valid_name "should return `true`: XT" true "XT";
        check_valid_name "should return `true`: YU" true "YU";
        check_valid_name "should return `true`: ZV" true "ZV";
        check_valid_name "should return `true`: IW" true "IW";
        check_valid_name "should return `true`: JX" true "JX";
        check_valid_name "should return `true`: KY" true "KY";
        check_valid_name "should return `true`: LZ" true "LZ"

    let test_invalid_one_character () =
        check_valid_name "should return false: ` `" false " ";
        check_valid_name "should return false: `_`" false "_";
        check_valid_name "should return false: `0`" false "0";
        check_valid_name "should return false: `1`" false "1";
        check_valid_name "should return false: `2`" false "2";
        check_valid_name "should return false: `3`" false "3";
        check_valid_name "should return false: `4`" false "4";
        check_valid_name "should return false: `5`" false "5";
        check_valid_name "should return false: `6`" false "6";
        check_valid_name "should return false: `7`" false "7";
        check_valid_name "should return false: `8`" false "8";
        check_valid_name "should return false: `9`" false "9";
        check_valid_name "should return false: `[`" false "[";
        check_valid_name "should return false: `]`" false "]";
        check_valid_name "should return false: `-`" false "-";
        check_valid_name "should return false: `+`" false "+";
        check_valid_name "should return false: `-`" false "-";
end



let suites: unit Alcotest.test list = [
    ("Utils/pp_value", [
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
