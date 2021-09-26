:- module(mover, [
    find_moves/4,
    same_color/2,
    board_replace/4,
    replace/4,
    chessman_location/3,
    board_free/3,
    valid_coordinates_move/2,
    valid_move/5,
    check/3,
    promote_pawn/4,
    move/5
]).

:- use_module(util).

%%%%%%%%%%%%%%%%%%%%
%% HULPPREDICATEN %%
%%%%%%%%%%%%%%%%%%%%

% De index van de rij waarop witte pionnen starten in een normaal schaakspel (volgens interne voorstelling van schaakbord)
white_pawns_starting_row(X) :-
    gameboard_size(Y),
    X is Y-2. % Index van 2de rij vanaf de onderkant van het spelbord

% Element uit de list op een bepaalde index (Puur omdat dit voor mij intuïtiever werkt)
element_at(Element, List, Index) :-
    nth0(Index, List, Element).

% Vervang element in lijst op een gegeven index
replace(List, Index, Element, ReplacedList) :-
    Dummy =.. [dummy|List],
    J is Index + 1,
    setarg(J, Dummy, Element), % Veel sneller dan recursive door rechtstreeks aan te passen
    Dummy =.. [dummy|ReplacedList].


% Speler en Schaakstuk hebben zelfde kleur.
same_color([color(Color), type(_)], current_player(Color)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BEWERKINGEN OP BORD EN METADATA %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Index van ieder kleur in de metadata-lijst
metadata_index(color(black), 0).
metadata_index(color(white), 1).

% Vervang een schaakstuk op een gegeven plaats
board_replace(BoardOrig, coordinate(Xorig, Yorig), Replacement, BoardUpdated) :-
    element_at(RowOrig, BoardOrig, Yorig), % Juiste rij selecteren voor verwijderen
    replace(RowOrig, Xorig, Replacement, RowRemoved), % Aanpassing doorvoeren in de rij
    replace(BoardOrig, Yorig, RowRemoved, BoardUpdated). % Aanpassing doorvoeren in Gameboard

% True als Chessman op de gegeven coordinaten op het bord staat
chessman_location(Gameboard, coordinate(X, Y), Chessman) :-
    nth0(Y, Gameboard, Row),
    nth0(X, Row, Chessman).

% True als er geen ander Schaakstuk op de gegeven locatie staat (eigen locatie wordt ook meegegeven)
board_free(_, coordinate(X, Y), coordinate(X, Y)). % Geval waarin wordt gekeken of de eigen plaats vrij is.
board_free(Board, coordinate(_, _), coordinate(Xfree, Yfree)) :-
    chessman_location(Board, coordinate(Xfree, Yfree), [color(none), type(empty)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ALGEMENE BEWEGINGSREGELS %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Controleert of de gegeven bron- en doelcoördinaten geldig zijn
valid_coordinates_move(coordinate(Xorig, Yorig), coordinate(Xnew, Ynew)) :-
    coordinate(Xorig, Yorig), % Geldig coördinaat?
    coordinate(Xnew, Ynew), % Geldig coördinaat?
    coordinate(Xorig, Yorig) \== coordinate(Xnew, Ynew). % Begincoördinaat is niet hetzelde als eindcoordinaat

% Geeft de richting waarin het Schaakstuk beweegt
coords_direction(coordinate(Xold, Yold), coordinate(Xnew, Ynew), [DirectionHorizontal, DirectionVertical]) :-
    xcoords_direction(Xold, Xnew, DirectionHorizontal),
    ycoords_direction(Yold, Ynew, DirectionVertical).

xcoords_direction(Xold, Xnew, direction(right)) :- min_list([Xold, Xnew], Xold). % We gaan naar rechts
xcoords_direction(Xold, Xnew, direction(left)) :- min_list([Xold, Xnew], Xnew). % We gaan naar links

ycoords_direction(Yold, Ynew, direction(down)) :- min_list([Yold, Ynew], Yold). % We gaan naar onder
ycoords_direction(Yold, Ynew, direction(up)) :- min_list([Yold, Ynew], Ynew). % We gaan naar onder

% Controleert of de diagonale beweging geldig is
diagonal_move(_, coordinate(X, Y), coordinate(X, Y), _).
diagonal_move(Board, coordinate(Xold, Yold), coordinate(Xnew, Ynew), [direction(right), direction(down)]) :-
    XnewMinus1 is Xnew-1,
    YnewMinus1 is Ynew-1,
    !,
    coordinate(XnewMinus1, YnewMinus1), % Controle of gecreëerd coordinaat binnen het spelbord valt
    board_free(Board, coordinate(Xold, Yold), coordinate(XnewMinus1, YnewMinus1)),
    diagonal_move(Board, coordinate(Xold, Yold), coordinate(XnewMinus1, YnewMinus1), [direction(right), direction(down)]).
diagonal_move(Board, coordinate(Xold, Yold), coordinate(Xnew, Ynew), [direction(right), direction(up)]) :-
    XnewMinus1 is Xnew-1,
    YnewPlus1 is Ynew+1,
    !,
    coordinate(XnewMinus1, YnewPlus1), % Controle of gecreëerd coordinaat binnen het spelbord valt
    board_free(Board, coordinate(Xold, Yold), coordinate(XnewMinus1, YnewPlus1)),
    diagonal_move(Board, coordinate(Xold, Yold), coordinate(XnewMinus1, YnewPlus1), [direction(right), direction(up)]).
diagonal_move(Board, coordinate(Xold, Yold), coordinate(Xnew, Ynew), [direction(left), direction(down)]) :-
    XnewPlus1 is Xnew+1,
    YnewMinus1 is Ynew-1,
    !,
    coordinate(XnewPlus1, YnewMinus1), % Controle of gecreëerd coordinaat binnen het spelbord valt,
    board_free(Board, coordinate(Xold, Yold), coordinate(XnewPlus1, YnewMinus1)),
    diagonal_move(Board, coordinate(Xold, Yold), coordinate(XnewPlus1, YnewMinus1), [direction(left), direction(down)]).
diagonal_move(Board, coordinate(Xold, Yold), coordinate(Xnew, Ynew), [direction(left), direction(up)]) :-
    XnewPlus1 is Xnew+1,
    YnewPlus1 is Ynew+1,
    !,
    coordinate(XnewPlus1, YnewPlus1), % Controle of gecreëerd coordinaat binnen het spelbord valt,
    board_free(Board, coordinate(Xold, Yold), coordinate(XnewPlus1, YnewPlus1)),
    diagonal_move(Board, coordinate(Xold, Yold), coordinate(XnewPlus1, YnewPlus1), [direction(left), direction(up)]).

horizontal_move(_, coordinate(X, Y), coordinate(X, Y), _).
horizontal_move(Board, coordinate(Xold, Yold), coordinate(Xnew, Ynew), direction(right)) :-
    XnewMinus1 is Xnew-1,
    !,
    coordinate(XnewMinus1, Ynew),
    board_free(Board, coordinate(Xold, Yold), coordinate(XnewMinus1, Ynew)),
    horizontal_move(Board, coordinate(Xold, Yold), coordinate(XnewMinus1, Ynew), direction(right)).
horizontal_move(Board, coordinate(Xold, Yold), coordinate(Xnew, Ynew), direction(left)) :-
    XnewPlus1 is Xnew+1,
    !,
    coordinate(XnewPlus1, Ynew),
    board_free(Board, coordinate(Xold, Yold), coordinate(XnewPlus1, Ynew)),
    horizontal_move(Board, coordinate(Xold, Yold), coordinate(XnewPlus1, Ynew), direction(left)).

vertical_move(_, coordinate(X, Y), coordinate(X, Y), _).
vertical_move(Board, coordinate(Xold, Yold), coordinate(Xnew, Ynew), direction(down)) :-
    YnewMinus1 is Ynew-1,
    !,
    coordinate(Xnew, YnewMinus1),
    board_free(Board, coordinate(Xold, Yold), coordinate(Xnew, YnewMinus1)),
    vertical_move(Board, coordinate(Xold, Yold), coordinate(Xnew, YnewMinus1), direction(down)).
vertical_move(Board, coordinate(Xold, Yold), coordinate(Xnew, Ynew), direction(up)) :-
    YnewPlus1 is Ynew+1,
    !,
    coordinate(Xnew, YnewPlus1),
    board_free(Board, coordinate(Xold, Yold), coordinate(Xnew, YnewPlus1)),
    vertical_move(Board, coordinate(Xold, Yold), coordinate(Xnew, YnewPlus1), direction(up)).

valid_target(Board, [Color, _], coordinate(Xnew, Ynew)) :- % Controleert of het schaakstuk op het doel genomen mag worden, indien het bestaat.
    chessman_location(Board, coordinate(Xnew, Ynew), [TargetColor, TargetType]),
    !,
    Color \== TargetColor,
    TargetType \== type(king).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Verplaatsingsregels per schaakstuk  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Toren
valid_move(Board, [Color, type(rook)], coordinate(Xold, Yold), coordinate(Xnew, Ynew), _) :-
    Xold == Xnew,
    !,
    valid_target(Board, [Color, type(rook)], coordinate(Xnew, Ynew)),
    ycoords_direction(Yold, Ynew, Direction),
    vertical_move(Board, coordinate(Xold, Yold), coordinate(Xnew, Ynew), Direction).
valid_move(Board, [Color, type(rook)], coordinate(Xold, Yold), coordinate(Xnew, Ynew), _) :-
    Yold == Ynew,
    !,
    valid_target(Board, [Color, type(rook)], coordinate(Xnew, Ynew)),
    xcoords_direction(Xold, Xnew, Direction),
    horizontal_move(Board, coordinate(Xold, Yold), coordinate(Xnew, Ynew), Direction).
% Paard
valid_move(Board, [Color, type(knight)], coordinate(Xold, Yold), coordinate(Xnew, Ynew), _) :-
    ( % Alle geldige combinaties voor het verzetten van een paard
        ((Xnew is Xold+2; Xnew is Xold-2), (Ynew is Yold+1; Ynew is Yold-1));
        ((Xnew is Xold+1; Xnew is Xold-1), (Ynew is Yold+2; Ynew is Yold-2))
    ),
    valid_target(Board, [Color, type(knight)], coordinate(Xnew, Ynew)).
% Loper
valid_move(Board, [Color, type(bishop)], coordinate(Xold, Yold), coordinate(Xnew, Ynew), _) :-
    coords_direction(coordinate(Xold, Yold), coordinate(Xnew, Ynew), Direction),
    valid_target(Board, [Color, type(bishop)], coordinate(Xnew, Ynew)),
    diagonal_move(Board, coordinate(Xold, Yold), coordinate(Xnew, Ynew), Direction).
% Koningin
valid_move(Board, [Color, type(queen)], coordinate(Xold, Yold), coordinate(Xnew, Ynew), _) :-
    valid_move(Board, [Color, type(bishop)], coordinate(Xold, Yold), coordinate(Xnew, Ynew), _).
valid_move(Board, [Color, type(queen)], coordinate(Xold, Yold), coordinate(Xnew, Ynew), _) :-
    valid_move(Board, [Color, type(rook)], coordinate(Xold, Yold), coordinate(Xnew, Ynew), _).
% Koning
valid_move(Board, [Color, type(king)], coordinate(Xold, Yold), coordinate(Xnew, Ynew), _) :- % Gewone zet van koning
    % Oude en nieuwe X-waarde mogen maximaal 1 van elkaar verschillen
    % Analoog voor Y-waarde
    minus(Xold, Xnew, Xdiff),
    minus(Yold, Ynew, Ydiff),
    Xabs is abs(Xdiff),
    Yabs is abs(Ydiff),
    Xabs =< 1,
    Yabs =< 1,
    !,
    valid_move(Board, [Color, type(queen)], coordinate(Xold, Yold), coordinate(Xnew, Ynew), _).
valid_move(Board, [color(Color), type(king)], coordinate(4, Y), coordinate(6, Y), SpecialMoves) :- % Rokade koningszijde
    !,
    % Juiste metadata selecteren uit lijst
    metadata_index(color(Color), IndexMetadata),
    element_at(Row, SpecialMoves, IndexMetadata),
    member([color(Color), type(king)], Row), % Controle of Rokade koningszijde mogelijk is
    % Controle of er geen Schaakstukken tussen koning en toren staan
    chessman_location(Board, coordinate(5, Y), [color(none), type(empty)]),
    chessman_location(Board, coordinate(6, Y), [color(none), type(empty)]),
    % Controle of koning nooit schaak staat
    not(check(Board, current_player(Color), SpecialMoves)), % Huidige plaats is veilig
    board_replace(Board, coordinate(5, Y), [color(Color), type(king)], BoardKingMoved1),
    not(check(BoardKingMoved1, current_player(Color), SpecialMoves)). % 1 plaats opgeschoven is veilig
    % (2 plaatsen wordt in move() getest)
valid_move(Board, [color(Color), type(king)], coordinate(4, Y), coordinate(2, Y), SpecialMoves) :- % Rokade koninginnenzijde
    !,
    % Juiste metadata selecteren uit lijst
    metadata_index(color(Color), IndexMetadata),
    element_at(Row, SpecialMoves, IndexMetadata),
    member([color(Color), type(queen)], Row), % Controle of Rokade koninginnenzijde mogelijk is
    % Controle of er geen Schaakstukken tussen koning en toren staan
    chessman_location(Board, coordinate(3, Y), [color(none), type(empty)]),
    chessman_location(Board, coordinate(2, Y), [color(none), type(empty)]),
    chessman_location(Board, coordinate(1, Y), [color(none), type(empty)]),
    % Controle of koning nooit schaak staat
    not(check(Board, current_player(Color), SpecialMoves)), % Huidige plaats is veilig
    board_replace(Board, coordinate(3, Y), [color(Color), type(king)], BoardKingMoved1),
    not(check(BoardKingMoved1, current_player(Color), SpecialMoves)). % 1 plaats opgeschoven is veilig
    % (2 plaatsen wordt in move() getest)

% Pion
valid_move(Board, [color(black), type(pawn)], coordinate(X, Yold), coordinate(X, Ynew), SpecialMoves) :- % Pion 1 vak vooruit
    Ynew is Yold+1,
    !,
    valid_pawn_target(Board, [color(black), type(pawn)], coordinate(X, Ynew), [direction(none), direction(down)], SpecialMoves).
valid_move(Board, [color(black), type(pawn)], coordinate(X, Yold), coordinate(X, Ynew), SpecialMoves) :- % Pion 2 vakken vooruit 
    Ynew is Yold+2,
    Yold == 1, % Pion mag 2 stappen zetten als het op de beginpositie staat
    !,
    Ycheck is Yold+1,
    board_free(Board, coordinate(X, Yold), coordinate(X, Ycheck)), % Het vak tussen begin en eindpositie is vrij
    valid_pawn_target(Board, [color(black), type(pawn)], coordinate(X, Ynew), [direction(none), direction(down)], SpecialMoves).
valid_move(Board, [color(black), type(pawn)], coordinate(Xold, Yold), coordinate(Xnew, Ynew), SpecialMoves) :-  % Pion schuine aanval
    Ynew is Yold+1, % Altijd 1 vak naar beneden
    minus(Xold, Xnew, Xdiff),
    Xabs is abs(Xdiff),
    Xabs == 1, % Altijd 1 vak naar links of rechts
    !,
    xcoords_direction(Xold, Xnew, Direction),
    valid_pawn_target(Board, [color(black), type(pawn)], coordinate(Xnew, Ynew), [Direction, direction(down)], SpecialMoves).
valid_move(Board, [color(white), type(pawn)], coordinate(X, Yold), coordinate(X, Ynew), SpecialMoves) :- % Pion 1 vak vooruit
    Ynew is Yold-1,
    !,
    valid_pawn_target(Board, [color(white), type(pawn)], coordinate(X, Ynew), [direction(none), direction(up)], SpecialMoves).
valid_move(Board, [color(white), type(pawn)], coordinate(X, Yold), coordinate(X, Ynew), SpecialMoves) :- % Pion 2 vakken vooruit 
    Ynew is Yold-2,
    white_pawns_starting_row(A),
    Yold == A, % Pion mag 2 stappen zetten als het op de beginpositie staat
    !,
    Ycheck is Yold-1,
    board_free(Board, coordinate(X, Yold), coordinate(X, Ycheck)), % Het vak tussen begin en eindpositie is vrij
    valid_pawn_target(Board, [color(white), type(pawn)], coordinate(X, Ynew), [direction(none), direction(up)], SpecialMoves).
valid_move(Board, [color(white), type(pawn)], coordinate(Xold, Yold), coordinate(Xnew, Ynew), SpecialMoves) :-  % Pion schuine aanval
    Ynew is Yold-1, % Altijd 1 vak naar boven
    minus(Xold, Xnew, Xdiff),
    Xabs is abs(Xdiff),
    Xabs == 1, % Altijd 1 vak naar links of rechts
    !,
    xcoords_direction(Xold, Xnew, Direction),
    valid_pawn_target(Board, [color(white), type(pawn)], coordinate(Xnew, Ynew), [Direction, direction(up)], SpecialMoves).

%% SPECIALE REGELS PION %%
% Controleert of het doel van de pion genomen mag worden, indien het bestaat
valid_pawn_target(Board, [_, type(pawn)], coordinate(Xnew, Ynew), [direction(none), _], _) :- % Kan geen ander schaakstuk pakken (enkel verticale beweging)
    !,
    chessman_location(Board, coordinate(Xnew, Ynew), [color(none), type(empty)]).
valid_pawn_target(Board, [Color, type(pawn)], coordinate(Xnew, Ynew), [_, _], _) :- % Schuine beweging, MOET dus wel ander schaakstuk pakken
    chessman_location(Board, coordinate(Xnew, Ynew), [TargetColor, TargetType]),
    other_color(Color, TargetColor), % Kan enkel schaakstukken pakken van het andere kleur (Opgelet: er bestaat ook de kleur 'none' voor lege vakken)
    TargetType \== type(king). % Kan natuurlijk geen koning pakken
valid_pawn_target(_, [color(black), type(pawn)], coordinate(Xnew, Ynew), [_, _], [_, SpecialMovesWhite]) :- % En Passant voor zwarte pion
    last(SpecialMovesWhite, coordinate(XEnPassant, YEnPassant)),
    Xnew == XEnPassant,
    Ynew == YEnPassant.
valid_pawn_target(_, [color(white), type(pawn)], coordinate(Xnew, Ynew), [_, _], [SpecialMovesBlack, _]) :- % En Passant voor witte pion
    last(SpecialMovesBlack, coordinate(XEnPassant, YEnPassant)),
    Xnew == XEnPassant,
    Ynew == YEnPassant.

%%%%%%%%%%%%%%%%%%%%%
%% Schaak-detectie %%
%%%%%%%%%%%%%%%%%%%%%

check(Board, current_player(Color), SpecialMoves) :-
    chessman_location(Board, coordinate(Xking, Yking), [color(Color), type(king)]), % Locatie eigen koning opvragen
    board_replace(Board, coordinate(Xking, Yking), [color(Color), type(pawn)], BoardSwappedKing), % Koning vervangen door pion, want koning kan niet genomen worden door de tegenstander. Vervolgens kunnen we controleren of er een vijandig Schaakstuk deze pion kan innemen.
    other_color(color(Color), color(ColorInversed)), % Kleur tegenstander
    chessman_location(BoardSwappedKing, coordinate(Xenemy, Yenemy), [color(ColorInversed), type(TypeEnemy)]), % Alle vijandige schaakstukken afgaan om te kijken of die de koning bedreigt
    valid_move(BoardSwappedKing, [color(ColorInversed), type(TypeEnemy)], coordinate(Xenemy, Yenemy), coordinate(Xking, Yking), SpecialMoves).  

%%%%%%%%%%%%%%%%%%%
%% Promotie Pion %%
%%%%%%%%%%%%%%%%%%%

promote_pawn(Gameboard, [color(Color), type(pawn)], coordinate(X, Y), BoardPromoted) :- % Altijd promotie naar Queen
    gameboard_size(Size),
    MaxIndex is Size-1,
    (Y == 0; Y == MaxIndex), % Pion zit op het uiterste van het bord
    !,
    board_replace(Gameboard, coordinate(X, Y), [color(Color), type(queen)], BoardPromoted).
promote_pawn(Gameboard, [color(_), type(_)], coordinate(_, _), Gameboard). % Geen promotie mogelijk


%%%%%%%%%%%%%%%%%%%%%
%% Speciale zetten %%
%%%%%%%%%%%%%%%%%%%%%

%% EN PASSANT %%
% Hoofdpredicaat om En Passant-bewegingen te verwerken, buiten het controleren of de zet geldig is (dat gebeurt in valid_move())
en_passant_update(Board, [color(Color), type(Type)], coordinate(Xorig, Yorig), coordinate(Xnew, Ynew), SpecialMovesOrig, SpecialMovesFinal, BoardFinal) :-
    en_passant_remove_pawn(Board, [color(Color), type(Type)], coordinate(Xnew, Ynew), SpecialMovesOrig, BoardFinal),
    en_passant_add(SpecialMovesOrig, coordinate(Xorig, Yorig), coordinate(Xnew, Ynew), [color(Color), type(Type)], SpecialMovesAdded),
    en_passant_clear(SpecialMovesAdded, current_player(Color), SpecialMovesFinal).

% Predicaat bevat de Y-waarden van wanneer een En Passant-beweging moet worden opgeslagen in de metadata
% Begin = Y-waarde van waar pion start, End = Y-waarde waar pion eindigt, Middle = Y-waarde daartussen
% Laatste parameter is de index van de metadata in de metadata-lijst ()
en_passant_y_values(Begin, End, Middle, 1) :- % Voor Wit
    gameboard_size(Size),
    Begin is Size-2,
    End is Size-4,
    Middle is Size-3.
en_passant_y_values(1, 3, 2, 0). % Voor Zwart

% Pion zet 2 stappen vooruit: updaten metadata
en_passant_add(SpecialMovesOrig, coordinate(X, Yorig), coordinate(X, Ynew), [color(_), type(pawn)], SpecialMovesNew) :-
    en_passant_y_values(Begin, End, Middle, SpecialMovesIndex),
    Yorig == Begin,
    Ynew == End,
    !,
    element_at(SpecialMovesColor, SpecialMovesOrig, SpecialMovesIndex), % Juiste metadata-lijst selecteren
    length(SpecialMovesColor, Length),
    LastIndex is Length-1, % Index van laatste element vinden in de metadata-lijst
    replace(SpecialMovesColor, LastIndex, coordinate(X, Middle), SpecialMovesColorUpdated), % En Passant-mogelijkheid toevoegen aan metadata
    replace(SpecialMovesOrig, SpecialMovesIndex, SpecialMovesColorUpdated, SpecialMovesNew). % Aanpassing doorvoeren in SpecialMoves
en_passant_add(SpecialMoves, _, _, _, SpecialMoves). % De huidige zet laat geen En Passant toe

% Weghalen eventuele metadata 'En Passant' van tegenstander
en_passant_clear([SpecialMovesOrigBlack, SMOW], current_player(white), [SpecialMovesNewBlack, SMOW]) :-
    length(SpecialMovesOrigBlack, Length),
    LastIndex is Length-1,
    replace(SpecialMovesOrigBlack, LastIndex, [], SpecialMovesNewBlack). % En Passant-mogelijkheid verwijderen uit metadata
en_passant_clear([SMOB, SpecialMovesOrigWhite], current_player(black), [SMOB, SpecialMovesNewWhite]) :-
    length(SpecialMovesOrigWhite, Length),
    LastIndex is Length-1,
    replace(SpecialMovesOrigWhite, LastIndex, [], SpecialMovesNewWhite). % En Passant-mogelijkheid verwijderen uit metadata

% Verwijdert de pion die gepakt werd via een En Passant-beweging van de tegenstander 
en_passant_remove_pawn(Board, [color(white), type(pawn)], coordinate(Xnew, Ynew), [SpecialMovesBlack, _], BoardRemoved) :-
    last(SpecialMovesBlack, coordinate(Xnew, Ynew)), % Pawn doet een En Passant-beweging
    !,
    YEnemyPawn is Ynew+1,
    board_replace(Board, coordinate(Xnew, YEnemyPawn), [color(none), type(empty)], BoardRemoved).
en_passant_remove_pawn(Board, [color(black), type(pawn)], coordinate(Xnew, Ynew), [_, SpecialMovesWhite], BoardRemoved) :-
    last(SpecialMovesWhite, coordinate(Xnew, Ynew)), % Pawn doet een En Passant-beweging
    !,
    YEnemyPawn is Ynew-1,
    board_replace(Board, coordinate(Xnew, YEnemyPawn), [color(none), type(empty)], BoardRemoved).
en_passant_remove_pawn(Board, _, _, _, Board).

%% CASTLING %%
castling_rook(Board, [color(Color), type(king)], coordinate(4, Y), coordinate(6, Y), BoardCastled) :- % Koningszijde
    !,
    board_replace(Board, coordinate(7, Y), [color(none), type(empty)], BoardNoRook),
    board_replace(BoardNoRook, coordinate(5, Y), [color(Color), type(rook)], BoardCastled).
castling_rook(Board, [color(Color), type(king)], coordinate(4, Y), coordinate(2, Y), BoardCastled) :- % Koninginnenzijde
    !,
    board_replace(Board, coordinate(0, Y), [color(none), type(empty)], BoardNoRook),
    board_replace(BoardNoRook, coordinate(3, Y), [color(Color), type(rook)], BoardCastled).
castling_rook(Board, _, _, _, Board).

% Metadata van Rokades updaten
castling_clear(Chessman, CoOrig, CoNew, SpecialMovesOrig, SpecialMovesNew) :-
    castling_clear_by_move(Chessman, CoOrig, CoNew, SpecialMovesOrig, SpecialMovesTemp),
    castling_clear_by_taken(Chessman, CoOrig, CoNew, SpecialMovesTemp, SpecialMovesNew).

% Metadata updaten nadat een toren is verzet
castling_clear_by_move([color(Color), type(king)], coordinate(_, _), _, SpecialMovesOrig, SpecialMovesNew) :- % Koning beweegt, geen rokade meer mogelijk
    !,
    remove_queen(color(Color), SpecialMovesOrig, SpecialMovesTemp),
    remove_king(color(Color), SpecialMovesTemp, SpecialMovesNew).
castling_clear_by_move([color(black), type(rook)], coordinate(0, 0), _, SpecialMovesOrig, SpecialMovesNew) :- % Toren Zwart linksboven beweegt
    !,
    remove_queen(color(black), SpecialMovesOrig, SpecialMovesNew).
castling_clear_by_move([color(black), type(rook)], coordinate(X, 0), _, SpecialMovesOrig, SpecialMovesNew) :- % Toren Zwart rechtsboven beweegt
    gameboard_size(Size),
    X is Size-1,
    !,
    remove_king(color(black), SpecialMovesOrig, SpecialMovesNew).
castling_clear_by_move([color(white), type(rook)], coordinate(0, X), _, SpecialMovesOrig, SpecialMovesNew) :- % Toren Wit linksonder beweegt
    gameboard_size(Size),
    X is Size-1,
    !,
    remove_queen(color(white), SpecialMovesOrig, SpecialMovesNew).
castling_clear_by_move([color(white), type(rook)], coordinate(X, X), _, SpecialMovesOrig, SpecialMovesNew) :- % Toren Wit rechtsonder beweegt
    gameboard_size(Size),
    X is Size-1,
    !,
    remove_king(color(white), SpecialMovesOrig, SpecialMovesNew).
castling_clear_by_move(_, _, _, SpecialMoves, SpecialMoves).

% Metadata updaten nadat een toren is genomen
castling_clear_by_taken([color(black), type(_)], _, coordinate(0, MaxIndex), SpecialMovesOrig, SpecialMovesNew) :- % Zwart neemt Linkse Toren van Wit
    gameboard_size(Size),
    MaxIndex is Size-1, % nieuw x-coordinaat en y-coordinaat zijn linksonder het bord -> Toren is gepakt
    !,
    remove_queen(color(white), SpecialMovesOrig, SpecialMovesNew).
castling_clear_by_taken([color(black), type(_)], _, coordinate(MaxIndex, MaxIndex), SpecialMovesOrig, SpecialMovesNew) :- % Zwart neemt Rechtse Toren van Wit
    gameboard_size(Size),
    MaxIndex is Size-1, % nieuw x-coordinaat en y-coordinaat zijn rechtsonder het bord -> Toren is gepakt
    !,
    remove_king(color(white), SpecialMovesOrig, SpecialMovesNew).
castling_clear_by_taken([color(white), type(_)], _, coordinate(0, 0), SpecialMovesOrig, SpecialMovesNew) :- % Wit neemt Linkse Toren van Zwart
    !,
    remove_queen(color(black), SpecialMovesOrig, SpecialMovesNew).
castling_clear_by_taken([color(white), type(_)], _, coordinate(MaxIndex, 0), SpecialMovesOrig, SpecialMovesNew) :- % Wit neemt Rechtse Toren van Zwart
    gameboard_size(Size),
    MaxIndex is Size-1, % nieuw x-coordinaat en y-coordinaat zijn rechtsonder het bord -> Toren is gepakt
    !,
    remove_king(color(black), SpecialMovesOrig, SpecialMovesNew).
castling_clear_by_taken(_, _, _, SpecialMoves, SpecialMoves).


% Rokademogelijk langs koningsvleugel verwijderen
remove_king(color(Color), SpecialMovesOrig, SpecialMovesNew) :-
    metadata_index(color(Color), IndexMetadata),
    element_at(Row, SpecialMovesOrig, IndexMetadata),
    element_at([color(Color), type(king)], Row, IndexKing),
    !,
    replace(Row, IndexKing, [color(none), type(empty)], RowUpdated),
    replace(SpecialMovesOrig, IndexMetadata, RowUpdated, SpecialMovesNew).
remove_king(_, SpecialMoves, SpecialMoves).

% Rokademogelijk langs koninginnenvleugel verwijderen
remove_queen(color(Color), SpecialMovesOrig, SpecialMovesNew) :-
    metadata_index(color(Color), IndexMetadata),
    element_at(Row, SpecialMovesOrig, IndexMetadata),
    element_at([color(Color), type(queen)], Row, IndexKing),
    !,
    replace(Row, IndexKing, [color(none), type(empty)], RowUpdated),
    replace(SpecialMovesOrig, IndexMetadata, RowUpdated, SpecialMovesNew).
remove_queen(_, SpecialMoves, SpecialMoves).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Predicaat om een beweging te proberen uitvoeren %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Geeft een lijst van alle mogelijke volgende bordconfiguraties ('zetten') terug voor een gegeven startconfiguratie
find_moves(BoardBegin, SpecialMovesBegin, CurrentPlayer, BoardConfigs) :-
    findall([SpecialMovesNew, BoardUpdated], move(BoardBegin, SpecialMovesBegin, CurrentPlayer, SpecialMovesNew, BoardUpdated), BoardConfigs).

% Probeert een zet uit en geeft een bord terug indien geldige zet
move(BoardOrig, SpecialMovesOrig, CurrentPlayer, SpecialMovesFinal, BoardFinal) :-
    valid_coordinates_move(coordinate(Xorig, Yorig), coordinate(Xnew, Ynew)),
    chessman_location(BoardOrig, coordinate(Xorig, Yorig), Chessman), % Type Schaakstuk zoeken
    same_color(Chessman, CurrentPlayer), % De speler verzet enkel een Schaakstuk van eigen kleur 
    valid_move(BoardOrig, Chessman, coordinate(Xorig, Yorig), coordinate(Xnew, Ynew), SpecialMovesOrig), % Controleren de gegeven zet een geldige zet is voor dat Schaakstuk
    % Zet doorvoeren in het bord
    board_replace(BoardOrig, coordinate(Xorig, Yorig), [color(none), type(empty)], BoardRemoved),
    board_replace(BoardRemoved, coordinate(Xnew, Ynew), Chessman, BoardMoved),
    % Metadata aanpassen
    en_passant_update(BoardMoved, Chessman, coordinate(Xorig, Yorig), coordinate(Xnew, Ynew), SpecialMovesOrig, SpecialMovesEnPassant, BoardEnPassant),
    castling_clear(Chessman, coordinate(Xorig, Yorig), coordinate(Xnew, Ynew), SpecialMovesEnPassant, SpecialMovesFinal),
    castling_rook(BoardEnPassant, Chessman, coordinate(Xorig, Yorig), coordinate(Xnew, Ynew), BoardCastled),
    not(check(BoardCastled, CurrentPlayer, SpecialMovesFinal)),
    promote_pawn(BoardCastled, Chessman, coordinate(Xnew, Ynew), BoardFinal).

% Wordt enkel gebruikt tijdens debugging om een specifieke move te testen
%move_debug(BoardOrig, SpecialMovesOrig, CurrentPlayer, coordinate(Xorig, Yorig), coordinate(Xnew, Ynew), SpecialMovesFinal, BoardFinal) :-
%    valid_coordinates_move(coordinate(Xorig, Yorig), coordinate(Xnew, Ynew)),
%    chessman_location(BoardOrig, coordinate(Xorig, Yorig), Chessman), % Type Schaakstuk zoeken
%    same_color(Chessman, CurrentPlayer), % De speler verzet enkel een Schaakstuk van eigen kleur 
%    valid_move(BoardOrig, Chessman, coordinate(Xorig, Yorig), coordinate(Xnew, Ynew), SpecialMovesOrig), % Controleren de gegeven zet een geldige zet is voor dat Schaakstuk
%    % Zet doorvoeren in het bord
%    board_replace(BoardOrig, coordinate(Xorig, Yorig), [color(none), type(empty)], BoardRemoved),
%    board_replace(BoardRemoved, coordinate(Xnew, Ynew), Chessman, BoardMoved),
%    % Metadata aanpassen
%    en_passant_update(BoardMoved, Chessman, coordinate(Xorig, Yorig), coordinate(Xnew, Ynew), SpecialMovesOrig, SpecialMovesEnPassant, BoardEnPassant),
%    castling_clear(Chessman, coordinate(Xorig, Yorig), coordinate(Xnew, Ynew), SpecialMovesEnPassant, SpecialMovesFinal),
%    castling_rook(BoardEnPassant, Chessman, coordinate(Xorig, Yorig), coordinate(Xnew, Ynew), BoardCastled),
%    not(check(BoardCastled, CurrentPlayer, SpecialMovesFinal)),
%    promote_pawn(BoardCastled, Chessman, coordinate(Xnew, Ynew), BoardFinal).
