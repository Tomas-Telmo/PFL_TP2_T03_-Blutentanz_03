:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(random)).


:- consult(board).
:- consult(playerSettings).
:- consult(utils).
:- consult(move).
%----------------------------------------START GAME----------------------------------------%


% Launch the game
play :-
    write('Welcome to blutentanz_03!'), nl, nl, nl,
    
    write('======================================='), nl,
    write('|              GAME TYPE              |'), nl,
    write('|=====================================|'), nl, 
    write('|== Player1(Blue) vs Player2(Orange)==|'), nl,
    write('|       1. Human vs Human             |'), nl,
    write('|       2. Human vs Computer          |' ),nl,
    write('|       3. Computer vs Human          |'), nl,
    write('|       4. Computer vs Computer       |'), nl,
    write('======================================='), nl,nl,nl,nl,
   
    write('Choose game type (1-4): '), 
    read_number(GameType),clear_buffer,
        
	initial_player_settings(GameType, _, _, Player1_Type, Player2_Type),

	write('======================================='), nl,
    write('|            GAME CONFIGS             |'), nl,
    write('======================================='), nl,

    write('       >Enter board width:           '), read_number(Width),clear_buffer, nl,
    write('       >Enter board height:          '), read_number(Height),clear_buffer, nl,
    write('       >Enter pieces per player:     '), read_number(PiecesPerPlayer),clear_buffer,nl,
    write('       >Enter pieces necessary to win:     '), read_number(PiecesNecessaryToWin),clear_buffer,nl,nl,nl,nl,
    
        

    initial_state(game_config(Width, Height), game_state(1, player1Info(Player1_Type, 0), player2Info(Player2_Type, 0),PiecesPerPlayer,PiecesNecessaryToWin, 1, boardInfo(Width, Height, Board) )),

    
    
    game_loop(game_state(1, player1Info(Player1_Type, 0), player2Info(Player2_Type, 0),PiecesPerPlayer,PiecesNecessaryToWin, 1, boardInfo(Width, Height, Board) )). 
    



    

%----------------------------------------GAME STATE----------------------------------------%

% beginning of a game
% initial_state(Config, State) initializes the game state.

% Config is a configuration term containing the width and height of the board and the number of pieces per player.
% State is the initial state of the game, with the first turn, players having the specified number of pieces, and the initial board setup.

 %initial_state(Config, Game_State),
        %config(Width, Height)
        %game_state(Round, Player1_Info, Player2_Info, PiecesNecessaryToWin, CurrentPlayer ,BoardInfo)),

    %game_state(ROUND, (Player1_Type,Player1_PiecesUnplaced, PLayer1_PiecesDelivered),(Player2_Type,Player2_PiecesUnplaced, PLayer2_PiecesDelivered),PiecesNecessaryToWin,CurrentPlayer, BoardInfo(Width,Height,Board) )),


  initial_state(game_config(Width, Height), game_state(_, player1Info(Player1_Type, 0), player2Info(Player2_Type, 0),PiecesPerPlayer,PiecesNecessaryToWin, _, boardInfo(Width, Height, Board) )) :-
	
    write('======================================='), nl,
    write('|            GAME SETTINGS...         |'), nl,
    write('======================================='), nl,
    format('Board Width: ~w\n', [Width]),nl,
    format('Board Height: ~w\n', [Height]),nl,
    format('Player 1: ~w\n', [Player1_Type]),nl,
    format('Player 2: ~w\n', [Player2_Type]),nl,
    format('Pieces per player: ~w\n', [PiecesPerPlayer]),nl,
    format('Pieces necessary to win: ~w\n', [PiecesNecessaryToWin]),nl,
    write('======================================='), nl,nl,nl,
    

	generate_initial_board(Width, Height, Board).

%----------------------------------------GAME LOOP----------------------------------------%
game_loop(GameState):-
    %game_over(+GameState, -Winner) fails here if game is not over, goes to next clause
    game_over(GameState, Winner), 

    %show_winner(+Winner) displays the winner
    show_winner(Winner), !. 


game_loop( game_state(Turn, player1Info(Player1_Type, PLayer1_PiecesDelivered), player2Info(Player2_Type, PLayer2_PiecesDelivered), PiecesPerPlayer,PiecesNecessaryToWin, CurrentPlayer, BoardInfo )):-
    
    %display_game(+GameState), displays the current game state
    display_game(game_state(Turn, player1Info(Player1_Type, PLayer1_PiecesDelivered), player2Info(Player2_Type, PLayer2_PiecesDelivered),PiecesPerPlayer,PiecesNecessaryToWin, CurrentPlayer, BoardInfo )),
    
    
    %PLAYER MOVES------
    choose_rotate_type(BoardInfo, BoardInfo_AfterRotate), 
    
    %player_move(MovesLeft,Game_State)
    player_move(3,game_state(Turn, player1Info(Player1_Type, PLayer1_PiecesDelivered), player2Info(Player2_Type, PLayer2_PiecesDelivered),PiecesPerPlayer,PiecesNecessaryToWin, CurrentPlayer, BoardInfo_AfterRotate ),game_state(Turn, player1Info(Player1_Type, PLayer1_PiecesDelivered), player2Info(Player2_Type, PLayer2_PiecesDelivered),PiecesPerPlayer,PiecesNecessaryToWin, CurrentPlayer, UpdatedBoardInfo )), 

    %BOT MOVES---------
    %choose_move(CurrentPlayer,BoardInfo_AfterRotate, 3),%WIP 

    %choose_move(+GameState, +Level, -Move). -> for bot
    % piece(CurrentPlayer, Piece), put_piece(CurrentBoard, X-Y, Piece, NewBoard), TODO
   
    
    switch_player(CurrentPlayer, NewPlayer),
    
    NewTurn is Turn + 1,

    game_loop(game_state(NewTurn, player1Info(Player1_Type, 0), player2Info(Player2_Type, 0),PiecesPerPlayer,PiecesNecessaryToWin, NewPlayer, UpdatedBoardInfo )).

%---------------------------------------GAME_OVER---------------------------------------------------%

game_over( game_state(_, player1Info(_, PiecesNecessaryToWin), _, _, PiecesNecessaryToWin, _, _ ), 1). % player1 wins
game_over( game_state(_, _,player2Info(_, PiecesNecessaryToWin), _, PiecesNecessaryToWin, _, _ ), 2).% player2 wins

%---------------------------------------SHOW_WINNER---------------------------------------------------%
show_winner(Winner):-
    write('======================================='), nl,
    write('|               GAME OVER             |'), nl,
    write('======================================='), nl,
    format('------> player~w wins!!!!!            ', [Winner]),nl,nl,nl,nl.


%-------------------------------------------DISPLAY_GAME-----------------------------------------------%
% CASE- PLAYER 1 turn
display_game(game_state(Turn, player1Info(_, PLayer1_PiecesDelivered), _, PiecesPerPlayer,_, 1, boardInfo(Width,Height,Board) )):-
    write('=============='), nl,
   format(' -> Turn ~w  ', [Turn]),nl,
    write('=============='), nl,
    write('========================================='), nl,
    write('|             PLAYER 1 (BLUE)           |'),nl,
    write('========================================='), nl,nl,

    format('Available pieces: ~w', [PiecesPerPlayer]),nl,
    format('Pieces delivered: ~w', [PLayer1_PiecesDelivered]),nl,nl,
    
    display_current_board(Width,Height,Board),nl,nl,nl.

% CASE- PLAYER 2 turn
display_game(game_state(Turn, _, player2Info(_, PLayer2_PiecesDelivered),PiecesPerPlayer,_, 2, boardInfo(Width,Height,Board))):-

    write('=============='), nl,
   format(' -> Turn ~w  ', [Turn]),nl,
    write('=============='), nl,
    write('========================================='), nl,
    write('|             PLAYER 2 (ORANGE)         |'),nl,
    write('========================================='), nl,nl,

    format('Available pieces: ~w', [PiecesPerPlayer]),nl,
    format('Pieces delivered: ~w', [PLayer2_PiecesDelivered]),nl,nl,
    
    display_current_board(Width,Height,Board),nl,nl,nl.

%-------------------------------------------Switch_Player-----------------------------------------------%
switch_player(1, 2).
switch_player(2, 1).

%-------------------------------------------CHOOSE_ROTATE_TYPE-----------------------------------------------%

choose_rotate_type(BoardInfo, BoardInfo_AfterRotate):-
    write('Choose a rotation type: '),nl,
    write('======================================='), nl,
    write('|       1. Rotate a row               |'), nl,
    write('|       2. Rotate a column            |'),nl,
    write('======================================='), nl,nl,

    write('Choose a rotation type (1-2): '), 
    read_number(RotateType),clear_buffer,

    rotate_row_or_column(RotateType,BoardInfo, BoardInfo_AfterRotate).




%------------------------------------------ROTATE_ROW_OR_COLUMN-----------------------------------------------%
%ROW
rotate_row_or_column(1,boardInfo(Width, Height, Board), boardInfo(Width, Height, NewBoard)):-
    
    format('Enter row number (1-~w): ', [Height]),
    read_number(Row),clear_buffer,

    rotate_specified_row(Board, Row, NewBoard),
    
    display_current_board(Width,Height,NewBoard).



%COLUMN
rotate_row_or_column(2,boardInfo(Width, Height, Board), boardInfo(Width, Height, NewBoard)):-
    format('Enter collumn number (1-~w): ', [Width]),
    read_number(Collumn),clear_buffer,

    rotate_specified_column(Board, Collumn, NewBoard),
    
    display_current_board(Width,Height,NewBoard).


%-------------------------------------------player_piece-----------------------------------------------%
%must be recursive until all moves are gone or a player wins

%player1 wins
player_move(_, game_state(_, player1Info(_, PiecesNecessaryToWin), _, _, PiecesNecessaryToWin, _, _ ),game_state(_, _,player2Info(_, PiecesNecessaryToWin), _, PiecesNecessaryToWin, _, _ )):- !.

%player2 wins
player_move(_, game_state(_, _,player2Info(_, PiecesNecessaryToWin), _, PiecesNecessaryToWin, _, _ ),game_state(_, _,player2Info(_, PiecesNecessaryToWin), _, PiecesNecessaryToWin, _, _ )):- !.

%no more moves
player_move(0,FinalGameState,FinalGameState):- !.

player_move(MovesLeft,GameState,FinalGamestate):-
    MovesLeft > 0,
    format('MOVES LEFT: ~w', [MovesLeft]),nl,
    %choose a piece and dest coords
    choose_piece(GameState, Piece), 
    enter_move(Piece, Move), %would need some sort of fail safe if move is invalid
    
    %recieve a move, update the gamestate as necessary and update the board
    move(GameState, Move, NewGameState), 
    NewMovesLeft is MovesLeft - 1,

    player_move(NewMovesLeft,NewGameState,FinalGamestate).



%-------------------------------------------CHOOSE_PIECE-----------------------------------------------%

%CASE- PLAYER 1
choose_piece(game_state(_, player1Info(_, PLayer1_PiecesDelivered),_,PiecesPerPlayer,_, 1, _ ), piece(1, PieceNbr)):-

    PiecesAvailable is PiecesPerPlayer - PLayer1_PiecesDelivered,
    
    write('======================================='), nl,
    write('|     == == Available pieces == ==    |'), nl,
    write('       [PLAYER 1 - PIECE_NUMBER]         '), nl,

    display_available_pieces(1,PiecesAvailable,1),nl,
    
    write('======================================='), nl,nl,

    format('Enter piece number (1 to ~w): ', [PiecesAvailable]),
    read_number(PieceNbr), clear_buffer.

%CASE PLAYER 2
choose_piece(game_state(_, _, player2Info(_, PLayer2_PiecesDelivered),PiecesPerPlayer,_, 2, _ ), piece(2, PieceNbr)):-

    PiecesAvailable is PiecesPerPlayer - PLayer2_PiecesDelivered,
    
    write('======================================='), nl,
    write('|     == == Available pieces == ==    |'), nl,
    write('       [PLAYER 2 - PIECE_NUMBER]         '), nl,

    display_available_pieces(2,PiecesAvailable,1),nl,
    
    write('======================================='), nl,nl,

    format('Enter piece number (1 to ~w): ', [PiecesAvailable]),
    read_number(PieceNbr), clear_buffer.


display_available_pieces(_, PiecesAvailable, CurrentPiece) :-
    CurrentPiece > PiecesAvailable, !.

display_available_pieces(Current_Player,PiecesAvailable,CurrentPiece):-

        format('      ~w. Piece ~w - ~w       ', [CurrentPiece, Current_Player, CurrentPiece]), nl,
        NextPiece is CurrentPiece + 1,
        display_available_pieces(Current_Player,PiecesAvailable,NextPiece).


%-------------------------------------------ENTER_MOVE-----------------------------------------------%
enter_move(piece(Current_Player, PieceNbr), move(piece(Current_Player, PieceNbr), dest(Row, Collumn, Color))):-

    format('--> Piece Choosen: ~w - ~w       ', [Current_Player, PieceNbr]), nl,

    write('======================================='), nl,
    write('|==   ==   Select target tile   ==   ==|'), nl,
    
    %write('Enter row number (1-~w): ', [Height]),nl,
    write('Enter row number: '),
    read_number(Row),clear_buffer,nl,

    %write('Enter collumn number (1-~w): ', [Width]),nl,
    write('Enter Collumn number: '),
    read_number(Collumn),clear_buffer,nl,

    write('Enter color {orange, blue, black}: '),
    read(Color),clear_buffer,

    write('======================================='), nl,nl,nl.


%------------------------------------------- MOVE -----------------------------------------------%
  move(game_state(Turn, player1Info(Player1_Type, P1_Delivered), player2Info(Player2_Type, P2_Delivered), PiecesPerPlayer, PiecesNecessaryToWin, CurrentPlayer, boardInfo(Width, Height, CurrentBoard) ), move(piece(Player, Piece), dest(Row, Col, Color)),
       game_state(Turn, player1Info(Player1_Type, P1_Delivered), player2Info(Player2_Type, P2_Delivered), PiecesPerPlayer, PiecesNecessaryToWin, CurrentPlayer, boardInfo(Width, Height, NewBoard) )):-
    
    %move_piece(CurrentBoard, game_config(Width, Height), Player, Player-Piece, Row-Col-Color, NewFinalBoard) 
    move_piece(CurrentBoard, game_config(Width, Height), Player, Player-Piece, Row-Col-Color, NewBoard),

    display_current_board(Width,Height,NewBoard).

%------------------------------------------- MOVE_PIECE -----------------------------------------------%   
   

