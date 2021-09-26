:- use_module('../src/mover').
:- begin_tests(movertest).

% Random gekozen, manueel gecontroleerd 
board_state([[[color(black),type(rook)],[color(black),type(knight)],[color(black),type(bishop)],[color(black),type(queen)],[color(black),type(king)],[color(black),type(bishop)],[color(black),type(knight)],[color(black),type(rook)]],[[color(black),type(pawn)],[color(black),type(pawn)],[color(black),type(pawn)],[color(black),type(pawn)],[color(black),type(pawn)],[color(black),type(pawn)],[color(black),type(pawn)],[color(black),type(pawn)]],[[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)]],[[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)]],[[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)]],[[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)]],[[color(white),type(pawn)],[color(white),type(pawn)],[color(white),type(pawn)],[color(white),type(pawn)],[color(white),type(pawn)],[color(white),type(pawn)],[color(white),type(pawn)],[color(white),type(pawn)]],[[color(white),type(rook)],[color(white),type(knight)],[color(white),type(bishop)],[color(white),type(queen)],[color(white),type(king)],[color(white),type(bishop)],[color(white),type(knight)],[color(white),type(rook)]]]).
board_updated_1([[[color(black),type(rook)],[color(black),type(knight)],[color(black),type(bishop)],[color(black),type(queen)],[color(black),type(king)],[color(black),type(bishop)],[color(black),type(knight)],[color(black),type(rook)]],[[color(black),type(pawn)],[color(black),type(pawn)],[color(black),type(pawn)],[color(black),type(pawn)],[color(black),type(pawn)],[color(black),type(pawn)],[color(black),type(pawn)],[color(black),type(pawn)]],[[color(none),type(empty)],[color(white),type(rook)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)]],[[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)]],[[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)]],[[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)]],[[color(white),type(pawn)],[color(white),type(pawn)],[color(white),type(pawn)],[color(white),type(pawn)],[color(white),type(pawn)],[color(white),type(pawn)],[color(white),type(pawn)],[color(white),type(pawn)]],[[color(white),type(rook)],[color(white),type(knight)],[color(white),type(bishop)],[color(white),type(queen)],[color(white),type(king)],[color(white),type(bishop)],[color(white),type(knight)],[color(white),type(rook)]]]).
board_updated_2([[[color(black),type(rook)],[color(black),type(knight)],[color(black),type(bishop)],[color(black),type(queen)],[color(black),type(king)],[color(black),type(bishop)],[color(black),type(knight)],[color(black),type(rook)]],[[color(black),type(pawn)],[color(black),type(pawn)],[color(black),type(pawn)],[color(black),type(pawn)],[color(black),type(pawn)],[color(black),type(pawn)],[color(black),type(pawn)],[color(black),type(pawn)]],[[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)]],[[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)]],[[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)]],[[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)]],[[color(white),type(pawn)],[color(white),type(pawn)],[color(black),type(king)],[color(white),type(pawn)],[color(white),type(pawn)],[color(white),type(pawn)],[color(white),type(pawn)],[color(white),type(pawn)]],[[color(white),type(rook)],[color(white),type(knight)],[color(white),type(bishop)],[color(white),type(queen)],[color(white),type(king)],[color(white),type(bishop)],[color(white),type(knight)],[color(white),type(rook)]]]).
board_move_tests([[[color(none),type(empty)],[color(black),type(knight)],[color(black),type(bishop)],[color(black),type(queen)],[color(black),type(king)],[color(none),type(empty)],[color(black),type(knight)],[color(black),type(rook)]],[[color(black),type(rook)],[color(black),type(pawn)],[color(black),type(pawn)],[color(black),type(pawn)],[color(none),type(empty)],[color(black),type(pawn)],[color(black),type(pawn)],[color(black),type(pawn)]],[[color(black),type(pawn)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)]],[[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(white),type(pawn)],[color(black),type(pawn)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)]],[[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(black),type(bishop)],[color(none),type(empty)],[color(white),type(pawn)],[color(none),type(empty)],[color(white),type(pawn)]],[[color(white),type(queen)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)]],[[color(white),type(pawn)],[color(none),type(empty)],[color(white),type(pawn)],[color(white),type(bishop)],[color(white),type(pawn)],[color(none),type(empty)],[color(white),type(pawn)],[color(none),type(empty)]],[[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(none),type(empty)],[color(white),type(king)],[color(white),type(bishop)],[color(white),type(knight)],[color(white),type(rook)]]]).


test(same_color) :- same_color([color(white), type(rook)], current_player(white)).
test(same_color) :- same_color([color(black), type(king)], current_player(black)).
test(same_color) :- not(same_color([color(none), type(empty)], current_player(black))).
test(same_color) :- not(same_color([color(black), type(queen)], current_player(white))).

test(replace) :- replace([1, 2, 3], 1, 5, [1, 5, 3]).
test(replace) :- replace([1, 2, 3], 2, 5, [1, 2, 5]).
test(replace) :- not(replace([1, 2, 3, 4], 1, 7, [1, 7, 2, 3, 4])).


test(board_replace_1) :-
    board_state(Board),
    board_updated_1(BoardUpdated),
    board_replace(Board, coordinate(1, 2), [color(white), type(rook)], BoardUpdated).
test(board_replace_2) :-
    board_state(Board),
    board_updated_2(BoardUpdated),
    board_replace(Board, coordinate(2, 6), [color(black), type(king)], BoardUpdated).
test(chessman_location) :-
    board_updated_1(Board),
    chessman_location(Board, coordinate(1, 2), [color(white), type(rook)]).
test(chessman_location) :-
    board_updated_1(Board),
    not(chessman_location(Board, coordinate(1, 2), [color(white), type(king)])).
test(chessman_location) :-
    board_updated_2(Board),
    chessman_location(Board, coordinate(2, 6), [color(black), type(king)]).
test(chessman_location) :-
    board_updated_2(Board),
    not(chessman_location(Board, coordinate(2, 6), [color(none), type(empty)])).

test(board_free, [nondet]) :-
    board_state(Board),
    board_free(Board, coordinate(1, 1), coordinate(1, 1)). % Eigen plaats is altijd vrij
test(board_free) :-
    board_state(Board),
    board_free(Board, coordinate(6, 6), coordinate(1, 2)). % Eigen plaats is altijd vrij

test(valid_coordinates_move) :- valid_coordinates_move(coordinate(4, 4), coordinate(5, 5)).
test(valid_coordinates_move) :- not(valid_coordinates_move(coordinate(4, 4), coordinate(4, 4))).
test(valid_coordinates_move) :- not(valid_coordinates_move(1, 2)).

test(valid_move_rook, [nondet]) :-
    board_move_tests(Board),
    valid_move(Board, [color(black), type(rook)], coordinate(0, 1), coordinate(0, 0), _).
test(valid_move_rook) :-
    board_move_tests(Board),
    not(valid_move(Board, [color(black), type(rook)], coordinate(1, 0), coordinate(5, 0), _)).
test(valid_move_rook) :-
    board_move_tests(Board),
    not(valid_move(Board, [color(black), type(rook)], coordinate(1, 1), coordinate(2, 2), _)).

test(valid_move_knight, [nondet]) :-
    board_move_tests(Board),
    valid_move(Board, [color(black), type(knight)], coordinate(1, 0), coordinate(2, 2), _).
test(valid_move_knight, [nondet]) :-
    board_move_tests(Board),
    valid_move(Board, [color(white), type(knight)], coordinate(6, 7), coordinate(5, 5), _).
test(valid_move_knight) :-
    board_move_tests(Board),
    not(valid_move(Board, [color(white), type(knight)], coordinate(6, 7), coordinate(6, 5), _)).

test(valid_move_bishop, [nondet]) :-
    board_move_tests(Board),
    valid_move(Board, [color(white), type(bishop)], coordinate(3, 6), coordinate(4, 5), _).
test(valid_move_bishop, [nondet]) :-
    board_move_tests(Board),
    valid_move(Board, [color(black), type(bishop)], coordinate(3, 4), coordinate(6, 7), _).
test(valid_move_bishop) :-
    board_state(Board),
    not(valid_move(Board, [color(black), type(bishop)], coordinate(3, 4), coordinate(5, 5), _)).

test(valid_move_queen, [nondet]) :-
    board_move_tests(Board),
    valid_move(Board, [color(white), type(queen)], coordinate(0, 5), coordinate(2, 7), _).
test(valid_move_queen, [nondet]) :-
    board_move_tests(Board),
    valid_move(Board, [color(white), type(queen)], coordinate(0, 5), coordinate(5, 5), _).
test(valid_move_queen) :-
    board_move_tests(Board),
    not(valid_move(Board, [color(white), type(queen)], coordinate(0, 5), coordinate(5, 6), _)).

test(valid_move_king, [nondet]) :-
    board_move_tests(Board),
    valid_move(Board, [color(black), type(king)], coordinate(4, 0), coordinate(5, 0), _).
test(valid_move_king) :-
    board_move_tests(Board),
    not(valid_move(Board, [color(black), type(king)], coordinate(4, 0), coordinate(4, 2), _)).
test(valid_move_king, [nondet]) :-
    board_move_tests(Board),
    board_replace(Board, coordinate(3, 0), [color(none), type(empty)], BoardNoQueen),
    valid_move(BoardNoQueen, [color(black), type(king)], coordinate(4, 0), coordinate(3, 0), _).
test(valid_move_king) :- % Koningszijde rokade
    board_move_tests(Board),
    board_replace(Board, coordinate(6, 0), [color(none), type(empty)], BoardCastlingNotPossible), % paard weghalen
    not(valid_move(BoardCastlingNotPossible, [color(black), type(king)], coordinate(4, 0), coordinate(6, 0), [[[color(none), type(empty)], [color(black), type(king)]], []])). % Koning staat tussentijds schaak
test(valid_move_king, [nondet]) :- % Koningszijde rokade
    board_move_tests(Board),
    board_replace(Board, coordinate(6, 0), [color(none), type(empty)], BoardCastlingNotPossible), % paard weghalen
    board_replace(BoardCastlingNotPossible, coordinate(0, 5), [color(none), type(empty)], BoardCastlingPossible), % Vijandige koningin weghalen
    valid_move(BoardCastlingPossible, [color(black), type(king)], coordinate(4, 0), coordinate(6, 0), [[[color(none), type(empty)], [color(black), type(king)]], []]).
test(valid_move_king, [nondet]) :- % Koninginnenzijde rokade
    board_move_tests(Board),
    board_replace(Board, coordinate(3, 0), [color(none), type(empty)], BoardCastlingNotPossible), % queen weghalen
    board_replace(BoardCastlingNotPossible, coordinate(2, 0), [color(none), type(empty)], BoardCastlingNotPossible2), % loper weghalen
    board_replace(BoardCastlingNotPossible2, coordinate(1, 0), [color(none), type(empty)], BoardCastlingNotPossible3), % paard weghalen
    board_replace(BoardCastlingNotPossible3, coordinate(0, 0), [color(black), type(rook)], BoardCastlingPossible), % toren terugzetten
    valid_move(BoardCastlingPossible, [color(black), type(king)], coordinate(4, 0), coordinate(2, 0), [[[color(black), type(queen)], [color(black), type(king)]], []]).

test(valid_move_pawn) :-
    board_move_tests(Board),
    valid_move(Board, [color(black), type(pawn)], coordinate(1, 1), coordinate(1, 3), _).
test(valid_move_pawn) :-
    board_move_tests(Board),
    valid_move(Board, [color(black), type(pawn)], coordinate(1, 1), coordinate(1, 2), _).
test(valid_move_pawn) :-
    board_move_tests(Board),
    valid_move(Board, [color(black), type(pawn)], coordinate(0, 2), coordinate(0, 3), _).
test(valid_move_pawn) :-
    board_move_tests(Board),
    not(valid_move(Board, [color(black), type(pawn)], coordinate(0, 2), coordinate(0, 4), _)).
test(valid_move_pawn) :-
    board_move_tests(Board),
    valid_move(Board, [color(white), type(pawn)], coordinate(2, 6), coordinate(2, 4), _).
test(valid_move_pawn) :-
    board_move_tests(Board),
    not(valid_move(Board, [color(white), type(pawn)], coordinate(0, 6), coordinate(0, 5), _)).
test(valid_move_pawn) :-
    board_move_tests(Board),
    not(valid_move(Board, [color(white), type(pawn)], coordinate(0, 6), coordinate(0, 4), _)).

test(check, [nondet]) :-
    board_move_tests(Board),
    board_replace(Board, coordinate(4, 0), [color(none), type(empty)], BoardNoKing), % king weghalen
    board_replace(BoardNoKing, coordinate(5, 0), [color(black), type(king)], BoardMovedKing), % king terugzetten
    check(BoardMovedKing, current_player(black), [[], []]).
test(check) :-
    board_move_tests(Board),
    not(check(Board, current_player(black), [[], []])).

test(promote_pawn) :-
    board_move_tests(Board),
    board_replace(Board, coordinate(1, 7), [color(black), type(pawn)], BoardPromotionPossible),
    promote_pawn(BoardPromotionPossible, [color(black), type(pawn)], coordinate(1, 7), BoardPromoted),
    board_replace(Board, coordinate(1, 7), [color(black), type(queen)], BoardPromoted).
test(promote_pawn) :-
    board_move_tests(Board),
    board_replace(Board, coordinate(1, 7), [color(black), type(rook)], BoardPromotionPossible),
    promote_pawn(BoardPromotionPossible, [color(black), type(rook)], coordinate(1, 7), BoardPromoted),
    not(board_replace(Board, coordinate(1, 7), [color(black), type(queen)], BoardPromoted)). % Geen promotion

test(move, [nondet]) :- % En passant
    board_move_tests(Board),
    % Bord opzetten voor En passant
    board_replace(Board, coordinate(5, 4), [color(black), type(pawn)], Board1),
    board_replace(Board1, coordinate(4, 4), [color(white), type(pawn)], BoardEnPassant),
    move(BoardEnPassant, [[], [[color(white), type(queen)] ,[color(white), type(king)], coordinate(4, 5)]], current_player(black), [[],[[color(white),type(queen)],[color(white),type(king)],[]]], BoardAfter),
    % Controleren of de En-passant ertussen zit
    board_replace(BoardEnPassant, coordinate(5, 4), [color(none), type(empty)], BoardAfter1),
    board_replace(BoardAfter1, coordinate(4, 4), [color(none), type(empty)], BoardAfter2),
    board_replace(BoardAfter2, coordinate(4, 5), [color(black), type(pawn)], BoardAfter). % Het en-passant-bord manueel nagemaakt
    
test(find_moves) :- 
    board_move_tests(Board),
    find_moves(Board, [[[color(none), type(empty)], [color(none), type(empty)]], [[color(none), type(empty)], [color(none), type(empty)]]], current_player(black), Configs),
    length(Configs, Size),
    Size == 31. % Yes, manueel alle zetten nageteld

:- end_tests(movertest).
:- run_tests.
:- halt(0).