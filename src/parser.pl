:- module(parser, [
    special_moves/2,
    parse_board/2,
    parse_char/2,
    parse_input/3,
    remove_last/2,
    take/3,
    trim/3,
    second_last/2,
    substrings_charlists/2,
    is_playing/2,
    filter_board/2,
    add_empty_spaces_board/2
    ]).

:- use_module(util).

%%%%%%%%%%%%%%%%%%%%
%% HULPPREDICATEN %%
%%%%%%%%%%%%%%%%%%%%

% Verwijder laatste element uit lijst
remove_last([], []) :- !, fail. % 0 elementen in lijst -> kan geen element verwijderen
remove_last([_], []) :- !. % 1 element in lijst
remove_last([Head|Tail], [Head|TailWithoutLast]) :- % Meerdere elementen in lijst
    remove_last(Tail, TailWithoutLast).

% Neem de eerste N elementen van de Lijst
take(OriginalList,N,FilteredList) :- findall(E, (nth1(I,OriginalList,E), I =< N), FilteredList).

% Verwijder eerste N elementen van de lijst
trim(List,N,Trimmed) :-
    append(P,Trimmed,List),
    length(P,N).

% True als X het voorlaatste element in de lijst is
second_last([X,_], X).
second_last([_|T], X) :- second_last(T, X).
second_last([], []).

% Extra chars tussen het spelbord en de extra info ivm. speciale zetten -> (Rijnummer, spatie, *spelbord*, spatie, '[')
extra_chars(4).

%%%%%%%%%%%%%%%%
%% PARSE BORD %%
%%%%%%%%%%%%%%%%

% Omzetting tussen Schaakstuk-'emoji' en interne voorstelling Schaakstuk
parse_char('♙', [color(white), type(pawn)]).
parse_char('♟', [color(black), type(pawn)]).
parse_char('\s', [color(none), type(empty)]).
parse_char('♜', [color(black), type(rook)]).
parse_char('♞', [color(black), type(knight)]).
parse_char('♝', [color(black), type(bishop)]).
parse_char('♖', [color(white), type(rook)]).
parse_char('♘', [color(white), type(knight)]).
parse_char('♗', [color(white), type(bishop)]).
parse_char('♛', [color(black), type(queen)]).
parse_char('♚', [color(black), type(king)]).
parse_char('♕', [color(white), type(queen)]).
parse_char('♔', [color(white), type(king)]).
parse_char(_, ignored).

% Parset een rij van het inputbord
parse_board_charlist([Character|[]], [Parsed]) :- parse_char(Character, Parsed).
parse_board_charlist([Character|Rest], [Parsed|Chessman]) :- 
    parse_char(Character, Parsed),
    parse_board_charlist(Rest, Chessman).

% Parset het inputbord
parse_board_internal([CharlistHead|[]], [ParsedCharlist]) :- 
    parse_board_charlist(CharlistHead, ParsedCharlistUnfiltered),
    include(is_valid_parse, ParsedCharlistUnfiltered, ParsedCharlist).
parse_board_internal([CharlistHead|Rest], [ParsedCharList|ParsedCharLists]) :-
    parse_board_charlist(CharlistHead, ParsedCharlistUnfiltered),
    include(is_valid_parse, ParsedCharlistUnfiltered, ParsedCharList),
    parse_board_internal(Rest, ParsedCharLists).
parse_board(CharLists, ParsedCharLists) :-
    parse_board_internal(CharLists, ParsedCharListsWithExtra),
    remove_last(ParsedCharListsWithExtra, ParsedCharLists).

% True als het geparsete character een voorstelling van een Schaakstuk is
is_valid_parse([color(_), type(_)]).

% Omzetten tussen lijst van strings en lijsten van characters
substrings_charlists([SubstringHead|[]], [Charlists]) :- string_chars(SubstringHead, Charlists).
substrings_charlists([SubstringHead|Substringtail], [Charlist|Charlists]) :-
    string_chars(SubstringHead, Charlist),
    substrings_charlists(Substringtail, Charlists).

% Filtert de input weg die niet nodig is om enkel het bord te parsen (bvb metadata)
filter_board([[_, _|BoardUnfilteredHead]|[]], [BoardFiltered]) :-
    gameboard_size(X),
    take(BoardUnfilteredHead, X, BoardFiltered).
filter_board([[_|[]]|Rest], [['\s']|RestFiltered]) :-
    filter_board(Rest, RestFiltered).
filter_board([[_, _|BoardUnfilteredHead]|Rest], [BoardFilteredHead|RestFiltered]) :-
    gameboard_size(X),
    take(BoardUnfilteredHead, X, BoardFilteredHead),
    filter_board(Rest, RestFiltered).

% Indien het inputbord geen spaties bevat op de lege plaatsen, worden die toegevoegd
add_empty_spaces_board([GameBoardNoSpaceRowsHead|[]], [GameBoardRowsHead]) :- add_empty_spaces_row(GameBoardNoSpaceRowsHead, GameBoardRowsHead).
add_empty_spaces_board([GameBoardNoSpacesRowsHead|GameBoardNoSpacesRowsTail], [GameBoardRowsHead|GameBoardRowsTail]) :-
    add_empty_spaces_row(GameBoardNoSpacesRowsHead, GameBoardRowsHead),
    add_empty_spaces_board(GameBoardNoSpacesRowsTail, GameBoardRowsTail).

% Spaties toevoegen in een rij van het inputspelbord
add_empty_spaces_row(GameBoardAlreadyFilled, GameBoardAlreadyFilled) :-
    gameboard_size(X),
    length(GameBoardAlreadyFilled, X).
add_empty_spaces_row(GameBoardRowNoSpaces, GameBoardRow) :-
    gameboard_size(X),
    length(GameBoardRowNoSpaces, Y),
    Y < X,
    append(GameBoardRowNoSpaces, ['\s'], GameBoardRowAdded),
    add_empty_spaces_row(GameBoardRowAdded, GameBoardRow).

%%%%%%%%%%%%%%%%%%%%
%% PARSE METADATA %%
%%%%%%%%%%%%%%%%%%%%

% Parset de speler die aan beurt is
is_playing(FirstRow, current_player(black)) :-
    last(FirstRow, '☚'). % Indien het laatste teken van de eerste rij een handje is.
is_playing(_, current_player(white)). % In het andere geval is Wit aan de beurt

% Parset de metadata van het spel
special_moves([FirstRow|Rest], [SpecialMovesBlack, SpecialMovesWhite]) :-
    second_last(Rest, SecondLastRow),
    special_moves_row(FirstRow, SpecialMovesBlack),
    special_moves_row(SecondLastRow, SpecialMovesWhite).

% Parset een rij waarop metadata staat
special_moves_row(Row, SpecialMoves) :-
    gameboard_size(X),
    extra_chars(Y),
    SumXY is X+Y,
    trim(Row, SumXY, TrimmedRow),
    special_moves_filter_end(TrimmedRow, SpecialMovesChars),
    not(length(SpecialMovesChars, 0)), % Enkel als de lijst niet leeg is
    parse_board_internal([SpecialMovesChars], [Castling]), % Parse van rokades
    parse_en_passant(SpecialMovesChars, EnPassant), % Parse van En Passant
    append([Castling, [EnPassant]], SpecialMoves).
special_moves_row(_, [[]]). % een lege lijst stelt voor dat er geen En Passant-mogelijkheden zijn

% Haalt de eindtekens ']' en '☚' weg
special_moves_filter_end(Row, RowFiltered) :-
    last(Row, '\u261A'), %% Laatste element van de rij is '☚'
    length(Row, N),
    take(Row, N-2, RowFiltered).
special_moves_filter_end(Row, RowFiltered) :-
    last(Row, ']'),
    length(Row, N),
    take(Row, N-1, RowFiltered).

parse_en_passant(Unparsed, coordinate(X, Y)) :-
    last(Unparsed, NumberAtom),
    atom_number(NumberAtom, YInversed),
    gameboard_size(Size),
    Y is Size-YInversed,
    second_last(Unparsed, Letter),
    letter_number(Letter, X).
parse_en_passant(_, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PARSE VOLLEDIGE INPUT %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Volledige parse van de input, 
parse_input(Board, SpecialMoves, current_player(PlayerColor)) :-
    read_string(user_input, _, InputBord),
    split_string(InputBord, '\n', '\n', SubStrings), %% TODO: check dit eens met char_type(newline) ofzo
    substrings_charlists(SubStrings, [CharListHead|CharListTail]),
    is_playing(CharListHead, current_player(PlayerColor)),
    special_moves([CharListHead|CharListTail], SpecialMoves),
    filter_board([CharListHead|CharListTail], GameBoardListsNoSpaces),
    add_empty_spaces_board(GameBoardListsNoSpaces, GameBoardLists),
    parse_board(GameBoardLists, Board).