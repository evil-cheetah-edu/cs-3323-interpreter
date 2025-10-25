(** Common helpers for interpreter tests. *)
module T     = Interpreter.Types
module IO    = Interpreter.Io
module Utils = Interpreter.Utils


(** A test case with an [expected] value and the actual [value] to compare. *)
type ('expected_t, 'value_t) test_case = {
    name:      string;
    expected: 'expected_t;
    value:    'value_t;
}


(** Helper function for comparing stacks *)
let eq_stack (left: T.stack) (right: T.stack) =
    left = right


type initial_stack_state = {
    initial_state: T.stack;
}


(** The Empty Stack. *)
let empty_stack: T.stack = []


(** Checks if the provided [value] (a stack) is the [Empty Stack].
    The test is named using [name]. *)
let check_empty ~name ~value =
    Alcotest.(check (Alcotest.testable IO.pp_stack eq_stack))
        name empty_stack value

(** Checks if the provided [value] (a stack) is equal to [expected] (a stack).
    The test is named using [name]. *)
let check_equal ~name ~expected ~value =
    Alcotest.(check (Alcotest.testable IO.pp_stack eq_stack))
        name expected value
