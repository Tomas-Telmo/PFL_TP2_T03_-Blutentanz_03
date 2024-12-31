move_put_piece(CurrentBoard, Player_Piece, NewBoard) :- 
    move_update_board(CurrentBoard, Player_Piece, 0-0, NewBoard).

move_update_board([], _, _, []).
move_update_board([Row | Rest], Player_Piece, TargetValue, [UpdatedRow | UpdatedRest]) :-
    move_update_row(Player_Piece, TargetValue, Row, UpdatedRow), 
    move_update_board(Rest, Player_Piece, TargetValue, UpdatedRest).

move_update_row(_, _, [], []) :- !. % Base case: empty row
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

board([
    [tile(1, 1, [(o-orange, 1-2), (g-black, 0-0), (b-blue, 0-0), (' '-black, 0-1)]),
     tile(1, 2, [(o-orange, 0-0), (g-black, 0-0), (b-blue, 0-0), (' '-black, 0-1)])],
    [tile(2, 1, [(o-orange, 0-0), (g-black, 0-0), (b-blue, 0-0), (' '-black, 0-1)]),
     tile(2, 2, [(o-orange, 0-0), (g-black, 0-0), (b-blue, 0-0), (' '-black, 0-1)])]
]).

% Example query to test the code
% board(Board), move_put_piece(Board, 1-2, NewBoard).
