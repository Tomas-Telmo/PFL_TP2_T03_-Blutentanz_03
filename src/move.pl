:- use_module(library(lists)).

%----------------------MAKE A MOVE---------------------------------------------% 
%-Move a piece inside the board
move_piece(CurrentBoard, game_config(Width, Height), Player, Player-Piece, Row-Col-Color, NewFinalBoard) :- 
    is_move_possible(CurrentBoard, game_config(Width, Height), Player, Player-Piece, Row-Col-Color),
    move_update_board(CurrentBoard, Player-Piece, 0-0, NewBoard),
    move_place_piece(NewBoard, Row-Col-Color, Player-Piece, NewFinalBoard),
    !.  

%Move a piece from inside the board to the board
move_put_piece(CurrentBoard, game_config(Width, Height), Player, Player-Piece, Row-Col-Color, NewFinalBoard) :-
    is_move_possible_start(CurrentBoard, game_config(Width, Height), Player, Player-Piece, Row-Col-Color),
    move_place_piece(Current_Player, Row-Col-Color, Player-Piece, NewFinalBoard),
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
%check move inside the board
is_move_possible(CurrentBoard, game_config(Width, Height), Current_Player, Current_Player-Piece, Row-Col-Color) :-
    within_bounds(game_config(Width, Height), Row-Col),
    empty_tile(CurrentBoard, Row-Col-Color),
    friendly_color(Current_Player, Row-Col-Color),
    is_close_tile(CurrentBoard, Current_Player-Piece, Row-Col-Color).

is_move_possible_start(CurrentBoard, game_config(Width, Height), Current_Player, Current_Player-Piece, Row-Col-Color) :-
    within_bounds(game_config(Width, Height), Row-Col),
    empty_tile(CurrentBoard, Row-Col-Color),
    friendly_color(Current_Player, Row-Col-Color),
    is_close_tile_start(CurrentBoard, game_config(Width, Height), Current_Player-Piece, Row-Col-Color).

%----------------------CHECK IF THE POSITION IS WITHIN BOUNDS---------------------------%
within_bounds(game_config(Width, Height), Row-Col) :-
    Row > 0,
    Col > 0,
    Row =< Height,
    Col =< Width.

%----------------------CHECK IF THE TILE IS EMPTY---------------------------%
empty_tile(CurrentBoard, Row-Col-Color) :-
    nth1(Row, CurrentBoard, TargetRow),
    empty_tile_aux(TargetRow, Col, Color). 

empty_tile_aux(Row, Col, Color) :- nth1(Col, Row, tile(_, _, [( _-Color, 0-0), _, _, _])).
empty_tile_aux(Row, Col, Color) :- nth1(Col, Row, tile(_, _, [ _,(_-Color, 0-0) , _, _])).
empty_tile_aux(Row, Col, Color) :- nth1(Col, Row, tile(_, _, [ _, _,( _-Color, 0-0), _])).
empty_tile_aux(Row, Col, Color) :- nth1(Col, Row, tile(_, _, [ _, _, _,( _-Color, 0-0)])).


%----------------------CHECK IF THE COLOR IS FRIENDLY---------------------------%
%Checks if the color of the tile is the same as the player's color or neutral%
friendly_color(1, _-_-blue).
friendly_color(_, _-_-black).
friendly_color(2, _-_-orange).


%----------------------CHECK IF THE TILE IS CLOSE TO THE PLAYER'S PIECE---------------------------%
%Checks if the tile is close to the player's piece(check the coordinates with find-player pice func)%
%check move inside the board
is_close_tile(CurrentBoard, Player-Piece, Row-Col-Color) :- 
    find_player_piece(CurrentBoard, Player-Piece, PlayerRow-PlayerCol, PlayerPosition),
    nth1(Row, CurrentBoard, TargetRow),
    nth1(Col, TargetRow, Tile),
    find_next(Tile, Color, NextPosition),
    friendly_tile(PlayerPosition, NextPosition),
    Row >= PlayerRow - 1,
    Row =< PlayerRow + 1,
    Col >= PlayerCol - 1,
    Col =< PlayerCol + 1.

is_close_tile_start(CurrentBoard,game_config(Width, Height), 1-Piece, 1-Col-Color) :-
    nth1(Row, CurrentBoard, TargetRow),
    nth1(Col, TargetRow, Tile),
    find_next(Tile, Color, NextPosition),
    start_friendly_tile(Player,NextPosition).

is_close_tile_start(CurrentBoard,game_config(Width, Height), 2-Piece, Height-Col-Color) :-
    nth1(Row, CurrentBoard, TargetRow),
    nth1(Col, TargetRow, Tile),
    find_next(Tile, Color, NextPosition),
    start_friendly_tile(Player,NextPosition).


%----------------------GET TOKEN's TILE----------------------------%
find_player_piece([], _, _, _) :- fail.
find_player_piece([Row | Rest], Player_Piece, RowNum-ColNum, Position) :- 
    find_player_piece_aux(Row, Player_Piece, RowNum-ColNum, Position);
    find_player_piece(Rest, Player_Piece, RowNum-ColNum, Position).

find_player_piece_aux([], _, _, _) :- fail.
find_player_piece_aux([tile(Row, Col, [(_, Player_Piece), _, _, _]) | _], Player_Piece, Row-Col, 1).
find_player_piece_aux([tile(Row, Col, [_, (_, Player_Piece), _, _]) | _], Player_Piece, Row-Col, 2).
find_player_piece_aux([tile(Row, Col, [_, _, (_, Player_Piece), _]) | _], Player_Piece, Row-Col, 3).
find_player_piece_aux([tile(Row, Col, [_, _, _, (_, Player_Piece)]) | _], Player_Piece, Row-Col, 4).
find_player_piece_aux([_ | Rest], Player_Piece, Row-Col, Position) :- 
    find_player_piece_aux(Rest, Player_Piece, Row-Col, Position).

find_next(tile(_, _, [(_-Color, _), _, _, _]), Color, 1).
find_next(tile(_, _, [ _,(_-Color, _), _, _]), Color, 2).
find_next(tile(_, _, [ _, _,(_-Color, _), _]), Color, 3).
find_next(tile(_, _, [ _, _, _,(_-Color, _)]), Color, 4).

%----------------------CHECK IF THE TILE PLACE IS FRIENDLY---------------------------%
%Checks if the tile is a friendly place to put the piece(if it iss on the direct side of the piece)%
friendly_tile(1,2).
friendly_tile(1,3).
friendly_tile(4,2).
friendly_tile(4,3).
friendly_tile(2,1).
friendly_tile(2,4).
friendly_tile(3,1).
friendly_tile(3,4).

%Checks if the move to the board is one of the closest to the border%
start_friendly_tile(1,3).
start_friendly_tile(1,4).
start_friendly_tile(2,1).
start_friendly_tile(2,2).


board([
    [tile(1, 1, [(g-black, 0-0), (o-orange, 0-0), (b-blue, 0-0), (' '-yellow, 0-1)]),
     tile(1, 2, [(g-black, 1-2),(o-orange, 0-0), (b-blue, 0-0), (' '-yellow, 0-1)])],
    [tile(2, 1, [(g-black, 0-0),(o-orange, 0-0), (b-blue, 0-0), (' '-yellow, 0-1)]),
     tile(2, 2, [(g-black, 0-0), (o-orange, 0-0) ,(b-blue, 0-0), (' '-yellow, 0-1)])]
]).
