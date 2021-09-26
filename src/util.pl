:- module(util, [
    gameboard_size/1,
    minus/3,
    other_color/2,
    coordinate/2,
    letter_number/2
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% DEZE MODULE BEVAT ENKELE HULPPREDICATEN DIE DOOR ANDERE MODULES GEBRUIKT WORDEN %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% De lengte van een zijde van het schaakbord
gameboard_size(8).

% Hulpfunctie om getallen van elkaar af te trekken
minus(A, B, C) :- % A - B = C
    plus(B, C, A).

% Kleuren omwisselen
other_color(color(white), color(black)).
other_color(color(black), color(white)).

% Interne voorstelling bord
% 0          7
%   ---------> x-as (kolommen)
%   |
%   |
%   |
% 7 v
%  y-as (rijen)
coordinate(Abscissa, Ordinate) :- % Stelt een coördinaat op het spelbord voor. Eerst de X-waarde en dan de Y-waarde.
    gameboard_size(X), % Index begint vanaf 0
    Xmin1 is X - 1,
    between(0, Xmin1, Abscissa),
    between(0, Xmin1, Ordinate).

% Omzetting van Letters naar Index van het spelbord
letter_number('a', 0).
letter_number('b', 1).
letter_number('c', 2).
letter_number('d', 3).
letter_number('e', 4).
letter_number('f', 5).
letter_number('g', 6).
letter_number('h', 7).