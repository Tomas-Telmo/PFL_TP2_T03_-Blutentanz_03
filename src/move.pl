put_piece(CurrentBoard, X-Y, Row-Column-Color, NewBoard) :- 
    update_board(CurrentBoard, X-Y, 0-0, TempBoard),
    update_board(TempBoard, Row-Column, X-Y-Color, NewBoard).


update_board(Board, Row-Column, NewValue, UpdatedBoard) :-
    nth1(Row, Board, OldRow),          % Get the row at position `Row`
    update_row(OldRow, Column, NewValue, UpdatedRow), % Update the column in the row
    replace(Board, Row, UpdatedRow, UpdatedBoard). % Replace the old row with the updated row

update_row(Row, Column, NewValue, UpdatedRow) :-
    nth1(Column, Row, _, Rest),       % Remove the element at `Column`, leaving `Rest`
    nth1(Column, UpdatedRow, NewValue, Rest). % Insert `NewValue` at `Column`

replace(List, Index, NewElement, NewList) :-
    nth1(Index, List, _, Rest),      % Remove the element at `Index`, leaving `Rest`
    nth1(Index, NewList, NewElement, Rest). % Insert `NewElement` at `Index`