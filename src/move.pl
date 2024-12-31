:- use_module(library(lists)).

%----------------------MAKE A MOVE---------------------------------------------%
% move_put_piece(+CurrentBoard, +Player-Piece, +Row-Col-Color, -NewFinalBoard)
% Move a piece from the player's hand to the board
move_put_piece(CurrentBoard, Player-Piece, Row-Col-Color, NewFinalBoard) :- 
    move_update_board(CurrentBoard, Player-Piece, 0-0, NewBoard),
    move_place_piece(NewBoard, Row-Col-Color, Player-Piece, NewFinalBoard),
    !.  

%----------------------TAKE PIECE OUT FROM ITS PLACE---------------------------%
% move_update_board(+Board, +Player-Piece, +TargetValue, -UpdatedBoard)
% Update the board to remove the piece from its current position
move_update_board([], _, _, []) :- !.
move_update_board([Row | Rest], Player-Piece, TargetValue, [UpdatedRow | UpdatedRest]) :- 
    move_update_row(Player-Piece, TargetValue, Row, UpdatedRow),
    move_update_board(Rest, Player-Piece, TargetValue, UpdatedRest).

% move_update_row(+Player-Piece, +TargetValue, +Row, -UpdatedRow)
% Update a row to remove the piece from its current position
move_update_row(_, _, [], []) :- !.  % Base case: empty row
move_update_row(Player-Piece, TargetValue, [tile(A, B, [(C, Player-Piece), D, E, F]) | Rest], [tile(A, B, [(C, TargetValue), D, E, F]) | UpdatedRest]) :-
    move_update_row(Player-Piece, TargetValue, Rest, UpdatedRest).

move_update_row(Player-Piece, TargetValue, [tile(A, B, [C, (D, Player-Piece), E, F]) | Rest], [tile(A, B, [C, (D, TargetValue), E, F]) | UpdatedRest]) :-
    move_update_row(Player-Piece, TargetValue, Rest, UpdatedRest).

move_update_row(Player-Piece, TargetValue, [tile(A, B, [C, D, (E, Player-Piece), F]) | Rest], [tile(A, B, [C, D, (E, TargetValue), F]) | UpdatedRest]) :-
    move_update_row(Player-Piece, TargetValue, Rest, UpdatedRest).

move_update_row(Player-Piece, TargetValue, [tile(A, B, [C, D, E, (F, Player-Piece)]) | Rest], [tile(A, B, [C, D, E, (F, TargetValue)]) | UpdatedRest]) :-
    move_update_row(Player-Piece, TargetValue, Rest, UpdatedRest).

move_update_row(Player-Piece, TargetValue, [tile(A, B, C) | Rest], [tile(A, B, C) | UpdatedRest]) :-
    move_update_row(Player-Piece, TargetValue, Rest, UpdatedRest).


%----------------------PLACE PIECE IN THE BOARD---------------------------%
% move_place_piece(+Board, +Row-Col-Color, +Player-Piece, -NewFinalBoard)
% Place the piece in the specified position on the board
move_place_piece(NewBoard, Row-Col-Color, Player-Piece, NewFinalBoard) :-
    nth1(Row, NewBoard, TargetRow),
    !,  
    place_piece_in_row(TargetRow, Col, Player-Piece, Color, UpdatedRow),
    replace_nth1(NewBoard, Row, UpdatedRow, NewFinalBoard).

% place_piece_in_row(+Row, +Col, +Player-Piece, +Color, -UpdatedRow)
% Place the piece in the specified position in the row
place_piece_in_row(Row, Col, Player-Piece, Color, UpdatedRow) :-
    nth1(Col, Row, Tile),
    replace_piece(Tile, Player-Piece, Color, UpdatedTile),
    replace_nth1(Row, Col, UpdatedTile, UpdatedRow).

% replace_piece(+Tile, +Player-Piece, +Color, -UpdatedTile)
% Replace the piece in the tile with the player's piece
replace_piece(tile(A, B, [(D-Color, _), G, E, F]), Player-Piece, Color, tile(A, B, [(D-Color, Player-Piece), G, E, F])) :- !.
replace_piece(tile(A, B, [G, (D-Color, _), E, F]), Player-Piece, Color, tile(A, B, [G, (D-Color, Player-Piece), E, F])) :- !.
replace_piece(tile(A, B, [G, E, (D-Color, _), F]), Player-Piece, Color, tile(A, B, [G, E, (D-Color, Player-Piece), F])) :- !.
replace_piece(tile(A, B, [G, E, F, (D-Color, _)]), Player-Piece, Color, tile(A, B, [G, E, F, (D-Color, Player-Piece)])) :- !.

replace_nth1([_|Rest], 1, Elem, [Elem|Rest]).
replace_nth1([H|Rest], N, Elem, [H|UpdatedRest]) :-
    N > 1,
    NextN is N - 1,
    replace_nth1(Rest, NextN, Elem, UpdatedRest).

%------------------------------------------------------------------------------%



%///////////////////////////////////////////////////////////////////////////////////////////////////%
%---------------------- CHECK POSSIBLE MOVE---------------------------------------------%
%///////////////////////////////////////////////////////////////////////////////////////////////////%

is_move_possible(CurrentBoard, Player-Piece, Row-Col-Color, NewFinalBoard):-















board([
    [tile(1, 1, [(o-orange, 1-2), (g-black, 0-0), (b-blue, 0-0), (' '-black, 0-1)]),
     tile(1, 2, [(o-orange, 0-0), (g-black, 0-0), (b-blue, 0-0), (' '-black, 0-1)])],
    [tile(2, 1, [(o-orange, 0-0), (g-black, 0-0), (b-blue, 0-0), (' '-black, 0-1)]),
     tile(2, 2, [(o-orange, 0-0), (g-black, 0-0), (b-blue, 0-0), (' '-black, 0-1)])]
]).
