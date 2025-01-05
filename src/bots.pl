% azul em baixo 1
% laranja em cima 1

:- consult(move).


valid_moves(game_state(_, player1Info(_,[Current_Piece | Rest], _), _, _, _, 1, boardInfo(Width,Height,Board)), AllPossibleMoves) :-
    process_all_pieces_moves([Current_Piece | Rest], Board, game_config(Width, Height), 1,AllPossibleMoves).


valid_moves(game_state(_, _, player2Info(_,[Current_Piece | Rest], _), _, _, 2, boardInfo(Width,Height,Board)), AllPossibleMoves) :-
    process_all_pieces_moves([Current_Piece | Rest], Board, game_config(Width, Height), 2,AllPossibleMoves).


process_all_pieces_moves([], _, _, _, []).
process_all_pieces_moves([Piece | Rest], Board, GameConfig, Player ,AllPossibleMoves) :-
    process_move_on_board_bot_tiles(Board, GameConfig, Player-Piece, Moves),
    process_all_pieces_moves(Rest, Board, GameConfig, Player ,RestMoves),
    append(Moves, RestMoves, AllPossibleMoves).




%----------------------GET TOKES TILE AND PLACE----------------------------%
%get_tokens_tile_and_place(+Board, +Player-Piece, -Row-Col, -Position)
find_bot_piece([], _, _, 100) :- !.
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
%process_move_off_board_bot_tiles(+Board, +game_config, +Player-Piece, -FinalMoves)
%gets the possible moves for pieces that are not inside the board yet
process_move_on_board_bot_tiles(Board, game_config(Width, Height), Player-Piece, Moves) :-
    process_move_off_board(Board, game_config(Width, Height), Player-Piece, Moves),
    !.

%process_move_on_board_bot_tiles(+Board, +game_config, +Player-Piece, -FinalMoves)
%gets the possible moves for pieces that are already inside the board
process_move_on_board_bot_tiles(Board, game_config(Width, Height), Player-Piece, FinalFinalMoves) :-
    get_close_tiles(Board, game_config(Width, Height), Player-Piece, CloseTiles),
    process_move_on_board(Board, CloseTiles, Player-Piece, FinalMoves),
    process_final_move_on_board(Board, game_config(Width, Height), Player-Piece, FinalMove),
    append(FinalMoves, FinalMove, FinalFinalMoves) , !.

%--------------------------------------------------------------------%



%----------------------FIND POSSIBLE MOVES FOR PIECE ALREADY ON BOARD----------------------------%
%process_moves_on_board(+Board, +Tiles, +Player-Piece, -Moves)
process_move_on_board(_, [], _, []) :- !.
process_move_on_board(Board, [tile(Row, Col, [( _-Color, 0-0), _, _, _]) | Rest], Player-Piece, [Piece-Row-Col-Color | Moves]) :- 
    find_bot_piece(Board, Player-Piece, Brow-Bcol, Position),
    bot_adapted_friendly_tile(Brow-Bcol-Position, Row-Col-1),
    friendly_color(Player, Row-Col-Color), !,
    process_move_on_board(Board, Rest, Player-Piece, Moves).

process_move_on_board(Board, [tile(Row, Col, [_, ( _-Color, 0-0), _, _]) | Rest], Player-Piece, [Piece-Row-Col-Color | Moves]) :- 
    find_bot_piece(Board, Player-Piece, Brow-Bcol, Position),
    bot_adapted_friendly_tile(Brow-Bcol-Position, Row-Col-2),
    friendly_color(Player, Row-Col-Color), !,
    process_move_on_board(Board, Rest, Player-Piece, Moves).

process_move_on_board(Board, [tile(Row, Col, [ _, _, ( _-Color, 0-0), _]) | Rest], Player-Piece, [Piece-Row-Col-Color | Moves]) :- 
    find_bot_piece(Board, Player-Piece, Brow-Bcol, Position),
    bot_adapted_friendly_tile(Brow-Bcol-Position, Row-Col-3),
    friendly_color(Player, Row-Col-Color), !,
    process_move_on_board(Board, Rest, Player-Piece, Moves).

process_move_on_board(Board, [tile(Row, Col, [_, _, _, ( _-Color, 0-0)]) | Rest], Player-Piece, [Piece-Row-Col-Color | Moves]) :- 
    find_bot_piece(Board, Player-Piece, Brow-Bcol, Position),
    bot_adapted_friendly_tile(Brow-Bcol-Position, Row-Col-4),
    friendly_color(Player, Row-Col-Color), !,
    process_move_on_board(Board, Rest, Player-Piece, Moves).

process_move_on_board(Board, [_UnmatchedTile | Rest], Player-Piece, Moves) :- 
    process_move_on_board(Board, Rest, Player-Piece, Moves).
%------------------------------------------------------------------------------------------------------------%


%------------------------------FIND POSSIBLE FINAL MOVE TO PIECE-------------------------------------%
%process_final_move_on_board(+Board, +game_config, +Player-Piece, -FinalMove)
process_final_move_on_board(Board, game_config(Width, _), 1-Piece, [Piece-FinalWidth-BotCol-blue]) :-
        find_bot_piece(Board, 1-Piece, BotRow-BotCol, _),
        is_finish_line(game_config(Width, _), BotRow-_-_),
        FinalWidth is Width + 1.

process_final_move_on_board(Board, game_config(Width, _), 2-Piece, [Piece-0-BotCol-orange]) :-        
        find_bot_piece(Board, 2-Piece, BotRow-BotCol, _),
        is_finish_line(game_config(Width, _), BotRow-_-_).

process_final_move_on_board(_, _, _, []).    
%------------------------------------------------------------------------------------------------------------%





%----------------------FIND POSSIBLE MOVES FOR PIECE THAT IS NOT YET ON THE BOARD----------------------------%
%find_possible_moves_for_piece(+Board, +Player-Piece, +Row-Col, +Position, -Moves)
process_move_off_board(Board, _, 1-Piece, Moves) :-
    find_bot_piece(Board, 1-Piece, _,100),
    nth1(1, Board, TargetRow),
    process_move_off_board_aux(TargetRow, 1-Piece, Moves).

process_move_off_board(Board, game_config(Width, _), 2-Piece, Moves) :-
    find_bot_piece(Board, 2-Piece, _,100),
    nth1(Width, Board, TargetRow),
    process_move_off_board_aux(TargetRow, 2-Piece, Moves).

process_move_off_board_aux([], _, []) :- !.
process_move_off_board_aux([tile(Row, Col, [(_-Color, 0-0) , _, _, _]) | Rest], 1-Piece, [Piece-1-Col-Color | Moves]) :- 
    friendly_color(1, Row-Col-Color), !,
    process_move_off_board_aux(Rest, 1-Piece, Moves).
process_move_off_board_aux([tile(Row, Col, [_, (_-Color, 0-0) , _, _]) | Rest], 1-Piece, [Piece-1-Col-Color | Moves]) :- 
    friendly_color(1, Row-Col-Color), !, 
    process_move_off_board_aux(Rest, 1-Piece, Moves).

process_move_off_board_aux([tile(Row, Col, [_, _, (_-Color, 0-0) , _]) | Rest], 2-Piece, [Piece-Row-Col-Color | Moves]) :-
    friendly_color(2, Row-Col-Color), !,
    process_move_off_board_aux(Rest, 2-Piece, Moves).
process_move_off_board_aux([tile(Row, Col, [_, _, _, (_-Color, 0-0)]) | Rest], 2-Piece, [Piece-Row-Col-Color | Moves]) :-
    friendly_color(2, Row-Col-Color), !,
    process_move_off_board_aux(Rest, 2-Piece, Moves).

process_move_off_board_aux([_ | Rest], Player-Piece, Moves) :-
    process_move_off_board_aux(Rest, Player-Piece, Moves).

%------------------------------------------------------------------------------------------------------------%




%------------------------------------------------------------------------------------------------------------%


%----------------------GET CLOSE TILES----------------------------%
%get_close_tiles(+Board, +game_config, +Player-Piece, -CloseTiles)
get_close_tiles(Board, game_config(Width, Height), Player-Piece, CloseTiles) :-
    find_bot_piece(Board, Player-Piece, Row-Col, _),
    Row1 is Row + 1,
    Row2 is Row - 1,
    Col1 is Col + 1,
    Col2 is Col - 1,
    findall(Tile, (
        member(Pos, [Row-Col, Row1-Col, Row2-Col, Row-Col1, Row-Col2]),
        within_bounds(game_config(Width, Height), Pos),
        get_tile(Board, Pos, Tile)
    ), CloseTiles).
%--------------------------------------------------------------------%

%----------------------GET SPECIFIED TILE----------------------------%
%get_tile(+Board, +Row-Col, -Tile)
get_tile(Board, Row-Col, Tile) :-
    nth1(Row, Board, TargetRow),
    nth1(Col, TargetRow, Tile). 
%--------------------------------------------------------------------%    

%----------------------AMIGOS DO 1----------------------------%    
bot_adapted_friendly_tile(Row-Col-1, Row-Col-2).
bot_adapted_friendly_tile(Row-Col-1, Row-Col-3).
bot_adapted_friendly_tile(Row-Col-1, OtherRow-Col-3) :- 
    OtherRow is Row - 1.
bot_adapted_friendly_tile(Row-Col-1, Row-OtherCol-2) :- 
    OtherCol is Col - 1.
%--------------------------------------------------------------------%
%----------------------AMIGOS DO 2----------------------------%
bot_adapted_friendly_tile(Row-Col-2, Row-Col-1).
bot_adapted_friendly_tile(Row-Col-2, Row-Col-4).
bot_adapted_friendly_tile(Row-Col-2, OtherRow-Col-4) :- 
    OtherRow is Row - 1.
bot_adapted_friendly_tile(Row-Col-2, Row-OtherCol-1) :-
    OtherCol is Col + 1.
%--------------------------------------------------------------------%
%----------------------AMIGOS DO 3----------------------------%
bot_adapted_friendly_tile(Row-Col-3, Row-Col-1).
bot_adapted_friendly_tile(Row-Col-3, Row-Col-4).
bot_adapted_friendly_tile(Row-Col-3, OtherRow-Col-1) :- 
    OtherRow is Row + 1.
bot_adapted_friendly_tile(Row-Col-3, Row-OtherCol-4) :- 
    OtherCol is Col - 1.
%--------------------------------------------------------------------%
%----------------------AMIGOS DO 4----------------------------%
bot_adapted_friendly_tile(Row-Col-4, Row-Col-2).
bot_adapted_friendly_tile(Row-Col-4, Row-Col-3).
bot_adapted_friendly_tile(Row-Col-4, OtherRow-Col-2) :- 
    OtherRow is Row + 1.
bot_adapted_friendly_tile(Row-Col-4, Row-OtherCol-3) :-
    OtherCol is Col + 1.
%--------------------------------------------------------------------%











