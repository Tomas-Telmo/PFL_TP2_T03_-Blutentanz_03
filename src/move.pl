move_put_piece(CurrentBoard, Player_Piece, Row-Column-Color, NewBoard) :- 
    move_update_board(CurrentBoard, Player_Piece, 0-0, TempBoard).

move_update_board([], _, _, []).
move_update_board([Row | Rest], Player_Piece, TargetValue, [UpdatedRow | UpdatedRest]) :-
    move_update_row(Player_Piece, TargetValue, Row, UpdatedRow), 
    move_update_board(Rest, Player_Piece, TargetValue, UpdatedRest).

move_update_row(_, _, [], []). % Base case: empty row
move_update_row(Player-Piece, TargetValue, [tile(_, _, [( _, Player-Piece), ( _, _), ( _, _), ( _, _)]) | Rest], [tile(_, _, [( _, TargetValue), ( _, _), ( _, _), ( _, _)])|Rest]).
move_update_row(Player-Piece, TargetValue, [tile(_, _, [( _, _), ( _, Player-Piece), ( _, _), ( _, _)]) | Rest], [tile(_, _, [( _, _), ( _, TargetValue), ( _, _), ( _, _)])|Rest]).
move_update_row(Player-Piece, TargetValue, [tile(_, _, [( _, _), ( _, _), ( _, Player-Piece), ( _, _)]) | Rest], [tile(_, _, [( _, _), ( _, _), ( _, TargetValue), ( _, _)])|Rest]).
move_update_row(Player-Piece, TargetValue, [tile(_, _, [( _, _), ( _, _), ( _, _), ( _, Player-Piece)]) | Rest], [tile(_, _, [( _, _), ( _, _), ( _, _), ( _, TargetValue)])|Rest]).
move_update_row(Player-Piece, TargetValue, [tile(X, Y, Tokens) | Rest], [tile(X, Y, Tokens) | UpdatedRest]) :-
    \+ member((_, Player-Piece), Tokens),
    move_update_row(Player-Piece, TargetValue, Rest, UpdatedRest).



board([
    [tile(1, 1, [(o-orange, 1-2), (g-black, 0-0), (b-blue, 0-0), (' '-black, 0-1)]),
     tile(1, 2, [(o-orange, 0-0), (g-black, 0-0), (b-blue, 0-0), (' '-black, 0-1)])],
    [tile(2, 1, [(o-orange, 0-0), (g-black, 0-0), (b-blue, 0-0), (' '-black, 0-1)]),
     tile(2, 2, [(o-orange, 0-0), (g-black, 0-0), (b-blue, 0-0), (' '-black, 0-1)])]
]).
