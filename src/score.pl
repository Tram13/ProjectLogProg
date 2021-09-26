:- module(score, [
    minimax/3
    ]).

:- use_module(mover).  % Voor find_moves/4
:- use_module(util). % Voor other_color/2

% Zoekdiepte van de spelboom
% Bepaalt de kwaliteit van de zoekresultaten, maar ook sterke invloed op uitvoeringstijd.
% Mag NIET kleiner zijn dan 1.
search_depth(3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% WAARDEN VAN 2 BORDEN VERGELIJKEN %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Score wordt vergeleken. De Modifier (1 of -1) zorgt ervoor dat we een minimum of maximum uitrekenen
compare_boards(BoardConfig1, Score1, _, Score2, Modifier, BoardConfig1, Score1) :-
    Score1Modified is Score1 * Modifier,
    Score2Modified is Score2 * Modifier,
    Score1Modified >= Score2Modified,
    !.
compare_boards(_, Score1, BoardConfig2, Score2, Modifier, BoardConfig2, Score2) :-
    Score1Modified is Score1 * Modifier,
    Score2Modified is Score2 * Modifier,
    Score1Modified < Score2Modified.

%%%%%%%%%%%%%%%%%%%%%
%% Score Berekenen %%
%%%%%%%%%%%%%%%%%%%%%

% Berekent de score van een bord
% Opmerking: Een goede score voor wit   is een HOGE score (positief)
%            Een goede score voor zwart is een LAGE score (negatief)
score([BoardRow|[]], RowValue) :- score_row(BoardRow, RowValue), !.
score([BoardRow|BoardRest], Value) :-
    score_row(BoardRow, RowValue),
    score(BoardRest, RestValue),
    Value is RowValue + RestValue,
    !.

% Berekent de score van een rij van het bord
score_row([Chessman|[]], ChessmanValue) :- score_chessman(Chessman, ChessmanValue).
score_row([Chessman|Chessmen], RowValue) :-
    score_chessman(Chessman, ChessmanValue),
    score_row(Chessmen, ChessmenValue),
    RowValue is ChessmanValue + ChessmenValue.

score_chessman([Color, Type], Value) :-
    worth(Type, AbsValue),
    color_modifier(Color, Modifier),
    Value is AbsValue * Modifier.

% Ieder Schaakstuk heeft een bepaalde waarde
% Scores gebaseerd op Wikipedia-artikel, geen eigen creatie (https://en.wikipedia.org/wiki/Chess_piece_relative_value)
min_worth(-200).
worth(type(pawn), 1).
worth(type(rook), 5).
worth(type(knight), 3).
worth(type(bishop), 3).
worth(type(queen), 9).
worth(_, 0). % Koning en lege vakken hebben geen score
% Schaakmat zorgt ervoor dat de huidige speler verliest -> Daarom een zo slecht mogelijke score genereren
worth(checkmate, current_player(Color), Value) :-
    min_worth(X),
    color_modifier(color(Color), Modifier),
    Value is X * Modifier.

% Positieve score voor wit
% Negatieve score voor zwart
color_modifier(color(white), 1) :- !.
color_modifier(color(black), -1) :- !.
color_modifier(_, 0).  % Lege vakken hebben geen score

%%%%%%%%%%%%%%%%%%%%%%%%
%% Spelboom opstellen %%
%%%%%%%%%%%%%%%%%%%%%%%%

% Maak de minimax-boom en bepaal de beste volgende zet
minimax(BoardConfig, current_player(white), BestBoardConfig) :-
    !,
    minimax_step(1, BoardConfig, current_player(white), 1, BestBoardConfig, _). % We starten op diepte 1: dit wil zeggen dat we alle mogelijke moves van het huidige bord gaan analyseren.
minimax(BoardConfig, current_player(black), BestBoardConfig) :-
    !,
    minimax_step(-1, BoardConfig, current_player(black), 1, BestBoardConfig, _).

% Een horizontale laag in de spelboom
% Indien de maximale diepte (search_depth/1) van de spelboom bereikt is, rekenen we de bladeren uit
minimax_step(Modifier, [SpecialMoves, Board], _, Depth, [SpecialMoves, Board], Score) :- % Berekening bladeren van minimax-boom
    search_depth(MaxDepth),
    Depth > MaxDepth,
    !,
    score(Board, ScoreNotModified),
    Score is ScoreNotModified * Modifier.
% Opstellen van minimax-boom, zolang de vereiste diepte (search_depth/1) niet bereikt is. (Recursief)
minimax_step(Modifier, [SpecialMoves, Board], CurrentPlayer, Depth, BestBoardConfig, BestScore) :-
    NewDepth is Depth + 1,
    find_moves(Board, SpecialMoves, CurrentPlayer, AllMoves),
    best_move(Modifier, AllMoves, CurrentPlayer, NewDepth, [SpecialMoves, Board], BestBoardConfig, BestScore).

%%%%%%%%%%%%%%%%%%%%%%%%
%% BESTE KIND BEPALEN %%
%%%%%%%%%%%%%%%%%%%%%%%%

% Zoekt de beste volgende zet uit een lijst van zetten (kinderen van een top uit minimax-boom, gemaakt door minimax_step/6)
best_move(_, [], CurrentPlayer, _, Previous, Previous, CheckmateScore) :- % Er zijn geen volgende zetten mogelijk (Schaakmat)
    !,
    worth(checkmate, CurrentPlayer, CheckmateScore).
best_move(Modifier, [BoardConfig|[]], current_player(PlayerColor), Depth, _, BoardConfig, BestScore) :- % Eindgeval
    !,
    NewModifier is Modifier * (-1),
    other_color(color(PlayerColor), color(OtherColor)),
    minimax_step(NewModifier, BoardConfig, current_player(OtherColor), Depth, _, BestScore).
best_move(Modifier, [[SpecialMoves, Board]|Rest], current_player(PlayerColor), Depth, _, BestBoardConfig, BestScore) :-
    NewModifier is Modifier * (-1),
    other_color(color(PlayerColor), color(OtherColor)),
	minimax_step(NewModifier, [SpecialMoves, Board], current_player(OtherColor), Depth, _, NewBestScore), % We hoeven niet te weten welk exact bord deze beste score opbracht
    best_move(Modifier, Rest, current_player(PlayerColor), Depth, _, TempBestBoardConfig, TempBestScore), % Voor ieder element van best_move input, tot de elementen op zijn, zoek ook daarvan steeds beste score
	compare_boards([SpecialMoves, Board], NewBestScore, TempBestBoardConfig, TempBestScore, Modifier, BestBoardConfig, BestScore). % Zoek het beste bord op de huidige diepte
