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
        check_pp_value "should return   '0': Integer   0"   "0" (T.Integer    0);
        check_pp_value "should return   '1': Integer   1"   "1" (T.Integer    1);
        check_pp_value "should return '100': Integer 100" "100" (T.Integer  100);
        check_pp_value "should return  '-9': Integer  -9"  "-9" (T.Integer (-9));
        check_pp_value "should return   '0': Integer  -0"   "0" (T.Integer (-0))
end


module TestResolveInt = struct
    let check_resolve_int name expected value =
        Alcotest.(check (Alcotest.option Alcotest.int)) name expected (To_Test.resolve_int value)
    
    let test_integers () =
        check_resolve_int "should return 0: Integer 0" (Some 0) (T.Integer 0);
        check_resolve_int "should return 6: Integer 6" (Some 6) (T.Integer 6);
        check_resolve_int "should return 55: Integer 55" (Some 55) (T.Integer 55);
        check_resolve_int "should return -1: Integer -1" (Some (-1)) (T.Integer (-1))
end


module TestIsValidName = struct
    let check_valid_name name expected value =
        Alcotest.(check bool) name expected (To_Test.is_valid_name value)

    let test_one_letter_lowercase_name () =
        check_valid_name "should return true: a" true "a";
        check_valid_name "should return true: b" true "b";
        check_valid_name "should return true: c" true "c";
        check_valid_name "should return true: d" true "d";
        check_valid_name "should return true: e" true "e";
        check_valid_name "should return true: f" true "f";
        check_valid_name "should return true: g" true "g";
        check_valid_name "should return true: h" true "h";
        check_valid_name "should return true: i" true "i";
        check_valid_name "should return true: j" true "j";
        check_valid_name "should return true: k" true "k";
        check_valid_name "should return true: l" true "l";
        check_valid_name "should return true: m" true "m";
        check_valid_name "should return true: n" true "n";
        check_valid_name "should return true: o" true "o";
        check_valid_name "should return true: p" true "p";
        check_valid_name "should return true: q" true "q";
        check_valid_name "should return true: r" true "r";
        check_valid_name "should return true: s" true "s";
        check_valid_name "should return true: t" true "t";
        check_valid_name "should return true: u" true "u";
        check_valid_name "should return true: v" true "v";
        check_valid_name "should return true: w" true "w";
        check_valid_name "should return true: x" true "x";
        check_valid_name "should return true: y" true "y";
        check_valid_name "should return true: z" true "z"

    let test_one_letter_uppercase_name () =
        check_valid_name "should return true: A" true "A";
        check_valid_name "should return true: B" true "B";
        check_valid_name "should return true: C" true "C";
        check_valid_name "should return true: D" true "D";
        check_valid_name "should return true: E" true "E";
        check_valid_name "should return true: F" true "F";
        check_valid_name "should return true: G" true "G";
        check_valid_name "should return true: H" true "H";
        check_valid_name "should return true: I" true "I";
        check_valid_name "should return true: J" true "J";
        check_valid_name "should return true: K" true "K";
        check_valid_name "should return true: L" true "L";
        check_valid_name "should return true: M" true "M";
        check_valid_name "should return true: N" true "N";
        check_valid_name "should return true: O" true "O";
        check_valid_name "should return true: P" true "P";
        check_valid_name "should return true: Q" true "Q";
        check_valid_name "should return true: R" true "R";
        check_valid_name "should return true: S" true "S";
        check_valid_name "should return true: T" true "T";
        check_valid_name "should return true: U" true "U";
        check_valid_name "should return true: V" true "V";
        check_valid_name "should return true: W" true "W";
        check_valid_name "should return true: X" true "X";
        check_valid_name "should return true: Y" true "Y";
        check_valid_name "should return true: Z" true "Z"

    let test_two_letter_lowercase_name () =
        check_valid_name "should return true: ea" true "ea";
        check_valid_name "should return true: fb" true "fb";
        check_valid_name "should return true: gc" true "gc";
        check_valid_name "should return true: hd" true "hd";
        check_valid_name "should return true: ie" true "ie";
        check_valid_name "should return true: jf" true "jf";
        check_valid_name "should return true: kg" true "kg";
        check_valid_name "should return true: lh" true "lh";
        check_valid_name "should return true: mi" true "mi";
        check_valid_name "should return true: nj" true "nj";
        check_valid_name "should return true: ok" true "ok";
        check_valid_name "should return true: pl" true "pl";
        check_valid_name "should return true: qm" true "qm";
        check_valid_name "should return true: rn" true "rn";
        check_valid_name "should return true: so" true "so";
        check_valid_name "should return true: tp" true "tp";
        check_valid_name "should return true: uq" true "uq";
        check_valid_name "should return true: vr" true "vr";
        check_valid_name "should return true: ws" true "ws";
        check_valid_name "should return true: xt" true "xt";
        check_valid_name "should return true: yu" true "yu";
        check_valid_name "should return true: zv" true "zv";
        check_valid_name "should return true: iw" true "iw";
        check_valid_name "should return true: jx" true "jx";
        check_valid_name "should return true: ky" true "ky";
        check_valid_name "should return true: lz" true "lz"

    let test_two_letter_uppercase_name () =
        check_valid_name "should return true: EA" true "EA";
        check_valid_name "should return true: FB" true "FB";
        check_valid_name "should return true: GC" true "GC";
        check_valid_name "should return true: HD" true "HD";
        check_valid_name "should return true: IE" true "IE";
        check_valid_name "should return true: JF" true "JF";
        check_valid_name "should return true: KG" true "KG";
        check_valid_name "should return true: LH" true "LH";
        check_valid_name "should return true: MI" true "MI";
        check_valid_name "should return true: NJ" true "NJ";
        check_valid_name "should return true: OK" true "OK";
        check_valid_name "should return true: PL" true "PL";
        check_valid_name "should return true: QM" true "QM";
        check_valid_name "should return true: RN" true "RN";
        check_valid_name "should return true: SO" true "SO";
        check_valid_name "should return true: TP" true "TP";
        check_valid_name "should return true: UQ" true "UQ";
        check_valid_name "should return true: VR" true "VR";
        check_valid_name "should return true: WS" true "WS";
        check_valid_name "should return true: XT" true "XT";
        check_valid_name "should return true: YU" true "YU";
        check_valid_name "should return true: ZV" true "ZV";
        check_valid_name "should return true: IW" true "IW";
        check_valid_name "should return true: JX" true "JX";
        check_valid_name "should return true: KY" true "KY";
        check_valid_name "should return true: LZ" true "LZ"

    let test_invalid_one_character () =
        check_valid_name "should return false: ' '" false "' '";
        check_valid_name "should return false: _"   false "_";
        check_valid_name "should return false: 0"   false "0";
        check_valid_name "should return false: 1"   false "1";
        check_valid_name "should return false: 2"   false "2";
        check_valid_name "should return false: 3"   false "3";
        check_valid_name "should return false: 4"   false "4";
        check_valid_name "should return false: 5"   false "5";
        check_valid_name "should return false: 6"   false "6";
        check_valid_name "should return false: 7"   false "7";
        check_valid_name "should return false: 8"   false "8";
        check_valid_name "should return false: 9"   false "9";
        check_valid_name "should return false: ["   false "[";
        check_valid_name "should return false: ]"   false "]";
        check_valid_name "should return false: -"   false "-";
        check_valid_name "should return false: +"   false "+";
        check_valid_name "should return false: -"   false "-";
end



let suites: unit Alcotest.test list = [
    ("Utils/pp_value", [
        Alcotest.test_case "integers" `Quick TestPPValue.test_integers;
    ]);

    ("Utils/resolve_int", [
        Alcotest.test_case "integers" `Quick TestResolveInt.test_integers;
    ]);

    ("Utils/is_valid_name", [
        Alcotest.test_case "one letter name (lowercase)" `Quick TestIsValidName.test_one_letter_lowercase_name;
        Alcotest.test_case "one letter name (uppercase)" `Quick TestIsValidName.test_one_letter_uppercase_name;
        Alcotest.test_case "two letter name (lowercase)" `Quick TestIsValidName.test_two_letter_lowercase_name;
        Alcotest.test_case "two letter name (uppercase)" `Quick TestIsValidName.test_two_letter_uppercase_name;
        Alcotest.test_case "invalid (one character)"     `Quick TestIsValidName.test_invalid_one_character;
    ])
]
