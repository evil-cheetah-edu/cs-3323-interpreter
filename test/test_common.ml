module T = Interpreter.Types

type ('expected_t, 'value_t) test_case = {
    name:      string;
    expected: 'expected_t;
    value:    'value_t;
}
