% azul em baixo 1
% laranja em cima 1

:- consult(move).


%----------------------GET TOKES TILE AND PLACE----------------------------%
% Finds the piece of the bot in the board
find_bot_piece([], _, _, not_on_board) :- !.
find_bot_piece([Row | Rest], Player_Piece, RowNum-ColNum, Position) :- 
    find_bot_piece_aux(Row, Player_Piece, RowNum-ColNum, Position), !;
    find_bot_piece(Rest, Player_Piece, RowNum-ColNum, Position).

find_bot_piece_aux([], _, _, _) :- fail.
find_bot_piece_aux([tile(Row, Col, [(_, Player_Piece), _, _, _]) | _], Player_Piece, Row-Col, 1).
find_bot_piece_aux([tile(Row, Col, [_, (_, Player_Piece), _, _]) | _], Player_Piece, Row-Col, 2).
find_bot_piece_aux([tile(Row, Col, [_, _, (_, Player_Piece), _]) | _], Player_Piece, Row-Col, 3).
find_bot_piece_aux([tile(Row, Col, [_, _, _, (_, Player_Piece)]) | _], Player_Piece, Row-Col, 4).
find_bot_piece_aux([_ | Rest], Player_Piece, Row-Col, Position) :- 
    find_bot_piece_aux(Rest, Player_Piece, Row-Col, Position).
%---------------------------------------------------------------------------%


%----------------------GET POSSIBLE MOVES----------------------------%
% goes trough all bot pieces and finds all possible moves
get_possible_moves(Board, Player, TotalPieceNum, PossibleMoves) :-
    get_possible_moves_aux(Board, Player, TotalPieceNum, 1, PossibleMoves).

get_possible_moves_aux(_, _, TotalPieceNum, CurrentPieceNum, []) :-
    CurrentPieceNum > TotalPieceNum, !.

get_possible_moves_aux(Board, Player, TotalPieceNum, CurrentPieceNum, AllMoves) :-
    find_bot_piece(Board, Player-CurrentPieceNum, RowNum-ColNum, Position),
    find_possible_moves_for_piece(Board, Player-CurrentPieceNum,RowNum-ColNum ,Position, Moves),
    NextPieceNum is CurrentPieceNum + 1,
    get_possible_moves_aux(Board, Player, TotalPieceNum, NextPieceNum, RestMoves),
    append(Moves, RestMoves, AllMoves).



%----------------------FIND POSSIBLE MOVES FOR PIECE----------------------------%
% finds all possible moves for a piece that is not on the board yet PLAYER 1
%find_possible_moves_for_piece(Board, game_config(width, height), 1-CurrentPieceNum, _, not_on_board, Moves) :- 
%    find_possible_moves_for_piece_start(Board, game_config(width, height), 1-CurrentPieceNum, Moves).


%find_possible_moves_for_piece_start(Board, game_config(width, height), 1-CurrentPieceNum, Moves) :-
%    nth1(height, Board, TargetRow).




%----------------------FIND POSSIBLE MOVES FOR PIECE ALREADY ON BOARD----------------------------%
process_move_on_board(_, [], _, []) :- !.
process_move_on_board(Board, [tile(Row, Col, [( _-Color, 0-0), _, _, _]) | Rest], Player-Piece, [Piece-Row-Col-Color | Moves]) :- 
    find_bot_piece(Board, Player-Piece, _, Position),
    friendly_tile(Position, 1),
    friendly_color(Player, Row-Col-Color),
    process_move_on_board(Board, Rest, Player-Piece, Moves).

process_move_on_board(Board, [tile(Row, Col, [_, ( _-Color, 0-0), _, _]) | Rest], Player-Piece, [Piece-Row-Col-Color | Moves]) :- 
    find_bot_piece(Board, Player-Piece, _, Position),
    friendly_tile(Position, 2),
    friendly_color(Player, Row-Col-Color),
    process_move_on_board(Board, Rest, Player-Piece, Moves).

process_move_on_board(Board, [tile(Row, Col, [ _, _, ( _-Color, 0-0), _]) | Rest], Player-Piece, [Piece-Row-Col-Color | Moves]) :- 
    find_bot_piece(Board, Player-Piece, _, Position),
    friendly_tile(Position, 3),
    friendly_color(Player, Row-Col-Color),
    process_move_on_board(Board, Rest, Player-Piece, Moves).

process_move_on_board(Board, [tile(Row, Col, [_, _, _, ( _-Color, 0-0)]) | Rest], Player-Piece, [Piece-Row-Col-Color | Moves]) :- 
    find_bot_piece(Board, Player-Piece, _, Position),
    friendly_tile(Position, 4),
    friendly_color(Player, Row-Col-Color),
    process_move_on_board(Board, Rest, Player-Piece, Moves).
%------------------------------------------------------------------------------------------------------------%

    





%----------------------FIND POSSIBLE MOVES FOR PIECE THAT IS NOT YET ON THE BOARD----------------------------%
%find_possible_moves_for_piece(+Board, +Player-Piece, +Row-Col, +Position, -Moves)
process_move_off_board([], _, []) :- !.
process_move_off_board([tile(Row, Col, [_, _, ( _-Color, 0-0), _]) | Rest], Player-Piece, [Piece-Row-Col-Color | Moves]) :-
    friendly_color(Player, Row-Col-Color),
    process_move_off_board(Rest, Player-Piece, Moves).

process_move_off_board([tile(Row, Col, [_, _, _, ( _-Color, 0-0)]) | Rest], Player-Piece, [Piece-Row-Col-Color | Moves]) :-
    friendly_color(Player, Row-Col-Color),
    process_move_off_board(Rest, Player-Piece, Moves).
%------------------------------------------------------------------------------------------------------------%


board([
    [tile(1, 1, [(g-black, 0-0), (o-orange, 0-0), (b-blue, 0-0), (' '-yellow, 0-1)]),
     tile(1, 2, [(g-black, 0-0),(o-orange, 0-0), (b-blue, 1-2), (' '-yellow, 0-1)])],
    [tile(2, 1, [(g-black, 0-0),(o-orange, 0-0), (b-blue, 0-0), (' '-yellow, 0-1)]),
     tile(2, 2, [(g-black, 0-0), (o-orange, 0-0) ,(b-blue, 0-0), (' '-yellow, 0-1)])]
]).