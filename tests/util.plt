:- use_module('../src/util').
:- begin_tests(utiltest).

test(minus) :- minus(3, 2, 1).
test(minus) :- not(minus(2, 3, 1)).
test(minus) :- minus(500, 260, 240).

test(other_color, [nondet]) :- other_color(color(white), color(black)).
test(other_color) :- not(other_color(color(black), color(grey))).
test(other_color) :- not(other_color(white, black)). % Missend predicaat: color/1
test(other_color) :- other_color(color(black), color(white)).

test(coordinate) :- coordinate(2, 3).
test(coordinate) :- coordinate(0, 0).
test(coordinate) :- not(coordinate(8, 8)).
test(coordinate) :- not(coordinate(8, 5)).
test(coordinate) :- not(coordinate(5, 8)).
test(coordinate) :- not(coordinate(-1, -1)).
test(coordinate) :- not(coordinate(-1, 5)).
test(coordinate) :- not(coordinate(5, -1)).

:- end_tests(utiltest).
:- run_tests.
:- halt(0).