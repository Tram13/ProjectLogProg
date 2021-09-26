:- initialization (main).
:- use_module(parser).
:- use_module(writer).
:- use_module(mover).
:- use_module(util).
:- use_module(score).

main :-
    % TEST MODE
    current_prolog_flag(argv, Flags),
    member('TEST', Flags),
    !,
    parse_input(Board, SpecialMoves, current_player(PlayerColor)),
    find_moves(Board, SpecialMoves, current_player(PlayerColor), AllMoves),
    other_color(color(PlayerColor), color(OpponentColor)),
    write_boards(AllMoves, current_player(OpponentColor)),
    halt(0).

main :-
    % GAME MODE
    parse_input(Board, SpecialMoves, current_player(PlayerColor)),
    minimax([SpecialMoves, Board], current_player(PlayerColor), [BestSpecialMoves, BestBoard]),
    %profile(minimax([SpecialMoves, Board], current_player(PlayerColor), [BestSpecialMoves, BestBoard])), % Profile de code om te zoeken naar mogelijke optimalisaties
    other_color(color(PlayerColor), color(OpponentColor)),
    write_board(BestBoard, current_player(OpponentColor), BestSpecialMoves),
    halt(0).
