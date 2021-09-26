:- use_module('../src/parser').
:- begin_tests(parsertest).

% Enkele kleine hulppredicaten
test(remove_last, [nondet]) :- not(remove_last([], [])). % Lege lijst moet falen
test(remove_last, [nondet]) :- not(remove_last([1], [2])). % Moet lege lijst zijn
test(remove_last) :- remove_last([1, 2, 4], [1, 2]).
test(remove_last) :- remove_last([1], []).

test(take) :-take([1, 2, 3, 4], 3, [1, 2, 3]).
test(take, [nondet]) :-not(take([1, 2, 3, 4], 2, [3, 4])). % Moet eerste 2 zijn ipv laatste 2
test(take) :-take([1, 2, 3, 4], 1, [1]).

test(trim) :- not(trim([1, 2, 3], 2, [1])). % Moet 3 zijn
test(trim, [nondet]) :- trim([1, 2, 3], 2, [3]).
test(trim, [nondet]) :- trim([1], 1, []).

test(second_last, [nondet]) :- second_last([1, 2, 3], 2).
test(second_last, [nondet]) :- second_last([1, 2], 1).
test(second_last, [nondet]) :- not(second_last([1, 2], 2)). % Moet 1 zijn

% Manueel gecontroleerde borden
% Beginopstelling
input_board_1("8 ♜♞♝♛♚♝♞♜ [♛♚]\n7 ♟♟♟♟♟♟♟♟\n6\n5\n4\n3\n2 ♙♙♙♙♙♙♙♙\n1 ♖♘♗♕♔♗♘♖ [♕♔]☚\n  abcdefgh").
output_board_1([[[color(black),type(rook)],[color(black),type(knight)],[color(black),type(bishop)],[color(black),type(queen)],[color(black),type(king)],[color(black),type(bishop)],[color(black),type(knight)],[color(black),type(rook)]],[[color(black),type(pawn)],[color(black),type(pawn)],[color(black),type(pawn)],[color(black),type(pawn)],[color(black),type(pawn)],[color(black),type(pawn)],[color(black),type(pawn)],[color(black),type(pawn)]],[[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)]],[[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)]],[[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)]],[[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)]],[[color(white),type(pawn)],[color(white),type(pawn)],[color(white),type(pawn)],[color(white),type(pawn)],[color(white),type(pawn)],[color(white),type(pawn)],[color(white),type(pawn)],[color(white),type(pawn)]],[[color(white),type(rook)],[color(white),type(knight)],[color(white),type(bishop)],[color(white),type(queen)],[color(white),type(king)],[color(white),type(bishop)],[color(white),type(knight)],[color(white),type(rook)]]]).
special_moves_1([[[color(black),type(queen)],[color(black),type(king)],[]],[[color(white),type(queen)],[color(white),type(king)],[]]]).

% Willekeurig gekozen bord
input_board_2("8  ♞♝♛♚ ♞♜ [ ♚]☚\n7 ♜♟♟♟ ♟♟♟\n6 ♟\n5    ♙♟\n4    ♝ ♙ ♙\n3 ♕\n2 ♙ ♙♗♙ ♙\n1     ♔♗♘♖ [ ♔]\n  abcdefgh").
output_board_2([[[color(none),type(empty)],[color(black),type(knight)],[color(black),type(bishop)],[color(black),type(queen)],[color(black),type(king)],[color(none),type(empty)],[color(black),type(knight)],[color(black),type(rook)]],[[color(black),type(rook)],[color(black),type(pawn)],[color(black),type(pawn)],[color(black),type(pawn)],[color(none),type(empty)],[color(black),type(pawn)],[color(black),type(pawn)],[color(black),type(pawn)]],[[color(black),type(pawn)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)]],[[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(white),type(pawn)],[color(black),type(pawn)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)]],[[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(black),type(bishop)],[color(none),type(empty)],[color(white),type(pawn)],[color(none),type(empty)],[color(white),type(pawn)]],[[color(white),type(queen)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)]],[[color(white),type(pawn)],[color(none),type(empty)],[color(white),type(pawn)],[color(white),type(bishop)],[color(white),type(pawn)],[color(none),type(empty)],[color(white),type(pawn)],[color(none),type(empty)]],[[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(white),type(king)],[color(white),type(bishop)],[color(white),type(knight)],[color(white),type(rook)]]]).
special_moves_2([[[color(none),type(empty)],[color(black),type(king)],[]],[[color(none),type(empty)],[color(white),type(king)],[]]]).

% Test volledige input 1
test(parse_input_1, [nondet]) :- 
    input_board_1(InputBord),
    split_string(InputBord, '\n', '\n', SubStrings),
    substrings_charlists(SubStrings, [CharListHead|CharListTail]),
    is_playing(CharListHead, current_player(PlayerColor)),
    special_moves([CharListHead|CharListTail], SpecialMoves),
    filter_board([CharListHead|CharListTail], GameBoardListsNoSpaces),
    add_empty_spaces_board(GameBoardListsNoSpaces, GameBoardLists),
    parse_board(GameBoardLists, Board),
    output_board_1(Board),
    special_moves_1(SpecialMoves),
    PlayerColor == white.

% Test volledige input 2
test(parse_input_2, [nondet]) :- 
    input_board_2(InputBord),
    split_string(InputBord, '\n', '\n', SubStrings),
    substrings_charlists(SubStrings, [CharListHead|CharListTail]),
    is_playing(CharListHead, current_player(PlayerColor)),
    special_moves([CharListHead|CharListTail], SpecialMoves),
    filter_board([CharListHead|CharListTail], GameBoardListsNoSpaces),
    add_empty_spaces_board(GameBoardListsNoSpaces, GameBoardLists),
    parse_board(GameBoardLists, Board),
    output_board_2(Board),
    special_moves_2(SpecialMoves),
    PlayerColor == black.

:- end_tests(parsertest).
:- run_tests.
:- halt(0).