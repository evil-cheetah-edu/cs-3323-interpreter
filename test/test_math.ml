module T = Interpreter.Types


module To_Test = struct
    let add = Interpreter.Math.add
    let sub = Interpreter.Math.sub
end


module TestAdd = struct
    
end


let suites: unit Alcotest.test list = [
    ("Math/add", [
        
    ]);

    ("Math/sub", [
        
    ]);
]