open Test_common


module Utils = Interpreter.Utils


let eq_stack (left: T.stack) (right: T.stack) = left = right


module To_Test = struct
    let push = Interpreter.Functions.push
    let pop = Interpreter.Functions.pop
end


module TestPush = struct

end


let suites: unit Alcotest.test list = [
    ("Functions/push", [
        
    ]);

    ("Functions/pop", [
        
    ]);
]