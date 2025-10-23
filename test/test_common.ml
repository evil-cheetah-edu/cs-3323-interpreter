module T = Interpreter.Types

type ('expected_t, 'value_t) test_case = {
    name:      string;
    expected: 'expected_t;
    value:    'value_t;
}

let eq_stack (left: T.stack) (right: T.stack) =
    left = right


type 'a initial_stack_state = {
    initial_state: T.stack;
}
