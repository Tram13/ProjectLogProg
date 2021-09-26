:- module(writer, [
    write_board/3,
    write_boards/2
]).

:- use_module(parser).
:- use_module(util).

%%%%%%%%%%%%%%%%%%
%% OUTPUT WRITE %%
%%%%%%%%%%%%%%%%%%

% Print het handje ('☚') op de juiste plaats
write_current_player(1, current_player(white)) :-
    write('☚').
write_current_player(Number, current_player(black)) :-
    gameboard_size(Size),
    Number == Size,
    write('☚').
write_current_player(_, _).

% Print de mogelijkheden tot rokade en 'En Passant' op de juiste plaats
write_special_moves(1, [_, SpecialMovesWhite]) :-
    write(' ['),
    write_special_moves_internal(SpecialMovesWhite),
    write(']').
write_special_moves(Number, [SpecialMovesBlack, _]) :-
    gameboard_size(Size),
    Number == Size,
    write(' ['),
    write_special_moves_internal(SpecialMovesBlack),
    write(']').
write_special_moves(_, _).

% Print character per character de juiste tekens van de rokade en 'En Passant'
write_special_moves_internal([Head|[]]) :-
    parse_char(Char, Head),
    write(Char).
write_special_moves_internal([Head|Tail]) :-
    parse_char(Char, Head),
    write(Char),
    write_special_moves_internal(Tail).
write_special_moves_internal([coordinate(X, YInverse)]) :-
    letter_number(Letter, X),
    write(Letter),
    gameboard_size(Size),
    Y is Size-YInverse,
    write(Y).
write_special_moves_internal(_). % In het geval dat er geen 'En Passant'-mogelijkheid is

% Print de metadata van het spel (Wie is aan de beurt? Welke speciale zetten zijn mogelijk?)
write_metadata(Number, CurrentPlayer, SpecialMoves) :-
    write_special_moves(Number, SpecialMoves),
    write_current_player(Number, CurrentPlayer).

% Bij testmode moeten alle mogelijk boards geprint worden
write_boards([], _) :- writeln("DRAW").
write_boards([[SpecialMoves, Board]|[]], CurrentPlayer) :- write_board(Board, CurrentPlayer, SpecialMoves).
write_boards([[SpecialMoves, Board]|Rest], CurrentPlayer) :-
    write_board(Board, CurrentPlayer, SpecialMoves),
    nl,
    write('~'),
    nl,
    write_boards(Rest, CurrentPlayer).

% Print het volledige spelbord met alle metadata
write_board(Board, CurrentPlayer, SpecialMoves) :-
    gameboard_size(Size),
    write_board_internal(Board, Size, CurrentPlayer, SpecialMoves).

% Print rij per rij het volledige spelbord, inclusief metadata indien dat nodig is op die rij
write_board_internal([Head|[]], Number, CurrentPlayer, SpecialMoves) :-
    write_row(Head, Number),
    write_metadata(Number, CurrentPlayer, SpecialMoves),
    write('\n'),
    writeln("  abcdefgh").
write_board_internal([Head|Tail], Number, CurrentPlayer, SpecialMoves) :-
    write_row(Head, Number),
    write_metadata(Number, CurrentPlayer, SpecialMoves),
    write('\n'),
    NumberMinus1 is Number-1,
    write_board_internal(Tail, NumberMinus1, CurrentPlayer, SpecialMoves).

% Print een rij van het spelbord (zonder metadata)
write_row(Row, Number) :-
    write(Number),
    write(" "),
    write_row(Row).
write_row([Head|[]]) :-
    write_chessman(Head).
write_row([Head|Tail]) :-
    write_chessman(Head),
    write_row(Tail).

% Print een schaakstuk
write_chessman(Chessman) :-
    parse_char(Icon, Chessman),
    write(Icon).
