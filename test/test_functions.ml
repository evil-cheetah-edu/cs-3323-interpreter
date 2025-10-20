module T = Interpreter.Types


module To_Test = struct
    let push = Interpreter.Functions.push
    let pop = Interpreter.Functions.pop
end


let suites: unit Alcotest.test list = [
    ("Functions/push", [
        
    ]);

    ("Functions/pop", [
        
    ]);
]