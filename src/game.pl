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
    write('======================================='), nl,nl,
   
    write('Choose game type (1-4): '),
    repeat,
    read_number(GameType), clear_buffer, 
    (between(1, 4, GameType) -> !, true ; write('Invalid input, try again'), nl, fail),

    nl,nl,

	initial_player_settings(GameType, _, _, Player1_Type, Player2_Type),

	write('======================================='), nl,
    write('|            GAME CONFIGS             |'), nl,
    write('======================================='), nl,


    write('    > Enter board width (1-20): '), 
    repeat,
    read_number(Width),clear_buffer, nl,
    (between(1, 20, Width) -> !, true ; write('Invalid input, try again'), nl,nl, fail),
    

    write('    > Enter board height, 2 rows are already accounted for player spawns (1-20): '), 
    repeat,
    read_number(Height),clear_buffer, nl,
    (between(1, 20, Height) -> !, true ; write('Invalid input, try again'), nl,nl, fail),
    
    
    write('    > Enter pieces per player (1-20): '), 
    repeat,
    read_number(PiecesPerPlayer),clear_buffer,nl,
    (between(1, 20, PiecesPerPlayer) -> !, true ; write('Invalid input, try again'), nl,nl, fail),
    
    
    format('    > Enter pieces necessary to win (1-~w): ',[PiecesPerPlayer]), 
    nl,
    repeat,
    read_number(PiecesNecessaryToWin),clear_buffer,nl,
    (between(1, PiecesPerPlayer, PiecesNecessaryToWin) -> !, true ; write('Invalid input, try again'), nl,nl, fail),
    
    nl,nl,nl,
    
    createPiecesList(PiecesPerPlayer, PiecesList),

    initial_state(game_config(Width, Height), game_state(1, player1Info(Player1_Type, PiecesList,0), player2Info(Player2_Type,PiecesList, 0),PiecesPerPlayer,PiecesNecessaryToWin, 1, boardInfo(Width, Height, Board) )),

    
    game_loop(game_state(1, player1Info(Player1_Type,PiecesList, 0), player2Info(Player2_Type,PiecesList, 0),PiecesPerPlayer,PiecesNecessaryToWin, 1, boardInfo(Width, Height, Board) )). 
    
%----------------------------------------CREATE PIECES LIST----------------------------------------%

createPiecesList(PiecesPerPlayer, PiecesList) :-
    createPiecesListHelper(PiecesPerPlayer, [], PiecesList).

createPiecesListHelper(0, Acc, Acc) :- !.
createPiecesListHelper(PiecesPerPlayer, Acc, PiecesList) :-
    PiecesPerPlayer > 0,
    NewPiecesPerPlayer is PiecesPerPlayer - 1,
    createPiecesListHelper(NewPiecesPerPlayer, [PiecesPerPlayer | Acc], PiecesList).

    
    

%----------------------------------------GAME STATE----------------------------------------%

% beginning of a game
% initial_state(Config, State) initializes the game state.

% Config is a configuration term containing the width and height of the board and the number of pieces per player.
% State is the initial state of the game, with the first turn, players having the specified number of pieces, and the initial board setup.

 %initial_state(Config, Game_State),
        %config(Width, Height)
        %game_state(Round, Player1_Info, Player2_Info, PiecesNecessaryToWin, CurrentPlayer ,BoardInfo)),

    %game_state(ROUND, (Player1_Type,Player1_PiecesUnplaced, PLayer1_PiecesDelivered),(Player2_Type,Player2_PiecesUnplaced, PLayer2_PiecesDelivered),PiecesNecessaryToWin,CurrentPlayer, BoardInfo(Width,Height,Board) )),


  initial_state(game_config(Width, Height), game_state(_, player1Info(Player1_Type, _, _), player2Info(Player2_Type,_, _),PiecesPerPlayer,PiecesNecessaryToWin, _, boardInfo(Width, Height, Board) )) :-
	
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


game_loop( game_state(Turn, player1Info(Player1_Type,Player1_Pieces_Available, PLayer1_PiecesDelivered), player2Info(Player2_Type,Player2_Pieces_Available, PLayer2_PiecesDelivered), PiecesPerPlayer,PiecesNecessaryToWin, CurrentPlayer, BoardInfo )):-
    
    %display_game(+GameState), displays the current game state
    display_game(game_state(Turn, player1Info(Player1_Type, Player1_Pieces_Available, PLayer1_PiecesDelivered), player2Info(Player2_Type,Player2_Pieces_Available, PLayer2_PiecesDelivered),PiecesPerPlayer,PiecesNecessaryToWin, CurrentPlayer, BoardInfo )),
    
    
    %PLAYER MOVES------
    choose_rotate_type(BoardInfo, BoardInfo_AfterRotate), 

    %player_move(MovesLeft,BoardInfo,Game_State,NEW_Game_State)
    player_move(3,
        game_state(Turn, player1Info(Player1_Type,Player1_Pieces_Available, PLayer1_PiecesDelivered), player2Info(Player2_Type, Player2_Pieces_Available, PLayer2_PiecesDelivered),PiecesPerPlayer,PiecesNecessaryToWin, CurrentPlayer, BoardInfo_AfterRotate ),
        game_state(Turn, player1Info(Player1_Type, NewPlayer1_Pieces_Available, NewPLayer1_PiecesDelivered), player2Info(Player2_Type,NewPlayer2_Pieces_Available, NewPLayer2_PiecesDelivered),PiecesPerPlayer,PiecesNecessaryToWin, CurrentPlayer, UpdatedBoardInfo )), 

    %BOT MOVES---------
    %choose_move(CurrentPlayer,BoardInfo_AfterRotate, 3),%WIP 

    %choose_move(+GameState, +Level, -Move). -> for bot
    % piece(CurrentPlayer, Piece), put_piece(CurrentBoard, X-Y, Piece, NewBoard), TODO

    switch_player(CurrentPlayer, NewPlayer),
    
    NewTurn is Turn + 1,

    game_loop(game_state(NewTurn, player1Info(Player1_Type, NewPlayer1_Pieces_Available, NewPLayer1_PiecesDelivered), player2Info(Player2_Type,NewPlayer2_Pieces_Available, NewPLayer2_PiecesDelivered),PiecesPerPlayer,PiecesNecessaryToWin, NewPlayer, UpdatedBoardInfo )).

%---------------------------------------GAME_OVER---------------------------------------------------%
game_over(game_state(_, player1Info(_, _, PiecesDelivered), _, _, PiecesNecessaryToWin, _, _), 1) :-
   
    PiecesDelivered >= PiecesNecessaryToWin.

game_over(game_state(_, _, player2Info(_, _, PiecesDelivered), _, PiecesNecessaryToWin, _, _), 2) :-
    
    PiecesDelivered >= PiecesNecessaryToWin.

%---------------------------------------SHOW_WINNER---------------------------------------------------%
show_winner(Winner):-
    write('======================================='), nl,
    write('|               GAME OVER             |'), nl,
    write('======================================='), nl,
    format('------> player~w wins!!!!!            ', [Winner]),nl,nl,nl,nl.


%-------------------------------------------DISPLAY_GAME-----------------------------------------------%
% CASE- PLAYER 1 turn
display_game(game_state(Turn, player1Info(_,Player1_Pieces_Available,PLayer1_PiecesDelivered), _, _,_, 1, boardInfo(Width,Height,Board) )):-
    
    length(Player1_Pieces_Available,AvailablePieces),
    write('=============='), nl,
   format(' -> Turn ~w  ', [Turn]),nl,
    write('=============='), nl,
    write('========================================='), nl,
    write('|             PLAYER 1 (BLUE)           |'),nl,
    write('========================================='), nl,nl,

    format('Available pieces: ~w', [AvailablePieces]),nl,
    format('Pieces delivered: ~w', [PLayer1_PiecesDelivered]),nl,nl,
    
    display_current_board(Width,Height,Board),nl,nl,nl.

% CASE- PLAYER 2 turn
display_game(game_state(Turn, _, player2Info(_,Player2_Pieces_Available ,PLayer2_PiecesDelivered),_,_, 2, boardInfo(Width,Height,Board))):-

    length(Player2_Pieces_Available,AvailablePieces),
    write('=============='), nl,
   format(' -> Turn ~w  ', [Turn]),nl,
    write('=============='), nl,
    write('========================================='), nl,
    write('|             PLAYER 2 (ORANGE)         |'),nl,
    write('========================================='), nl,nl,

    format('Available pieces: ~w', [AvailablePieces]),nl,
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
    repeat,
    read_number(RotateType),clear_buffer,
    (between(1, 2, RotateType) -> !, true ; write('Invalid input, try again'), nl,nl, fail),

    rotate_row_or_column(RotateType,BoardInfo, BoardInfo_AfterRotate).



%------------------------------------------ROTATE_ROW_OR_COLUMN-----------------------------------------------%
%ROW
rotate_row_or_column(1,boardInfo(Width, Height, Board), boardInfo(Width, Height, NewBoard)):-
    
    Board_Heigth is Height + 1,
    format('Enter row number, ignoring player spawns (2-~w): ', [Board_Heigth]),
    repeat,
    read_number(Row),clear_buffer,
    (between(2, Board_Heigth, Row) -> !, true ; write('Invalid input, try again'), nl,nl, fail),


    translate_row_input(Row, Height, RealRow),

    rotate_specified_row(Board, RealRow, NewBoard),
    
    display_current_board(Width,Height,NewBoard), nl,nl.



%COLUMN
rotate_row_or_column(2,boardInfo(Width, Height, Board), boardInfo(Width, Height, NewBoard)):-
   
    format('Enter collumn number (1-~w): ', [Width]),
    repeat,
    read_number(Collumn),clear_buffer,
    (between(1, Width, Collumn) -> !, true ; write('Invalid input, try again'), nl,nl, fail),

    rotate_specified_column(Board, Collumn, NewBoard),
    
    display_current_board(Width,Height,NewBoard).


%-------------------------------------------player_piece-----------------------------------------------%
%must be recursive until all moves are gone or a player wins

%player1 wins
player_move(_, 
game_state(_, player1Info(_,_, PiecesDelivered), _, _, PiecesNecessaryToWin, _, _ ), 
game_state(_, player1Info(_,_, PiecesDelivered), _, _, PiecesNecessaryToWin, _, _ )) :- 
   PiecesDelivered >= PiecesNecessaryToWin, !.

%player2 wins
player_move(_, 
game_state(_, _, player2Info(_,_, PiecesDelivered), _, _, PiecesNecessaryToWin, _, _ ), 
game_state(_, _, player2Info(_,_, PiecesDelivered), _, _, PiecesNecessaryToWin, _, _ )) :- 
    PiecesDelivered >= PiecesNecessaryToWin, !.

%no more moves
player_move(0,FinalGameState,FinalGameState):- !.

player_move(MovesLeft, GameState, FinalGameState) :-
    MovesLeft > 0,
    format('MOVES LEFT: ~w', [MovesLeft]), nl,
    % choose a piece and destination coordinates
    choose_piece(GameState, Piece),
   
    enter_move(GameState, Piece, Move),
    
    (check_move(GameState, Move) 
    ->
        % if the move is valid, update the game state
        move(GameState, Move, NewGameState),
        NewMovesLeft is MovesLeft - 1,
        player_move(NewMovesLeft, NewGameState, FinalGameState)
    ;
        % if the move is invalid, restart the process
        write('Invalid move, restarting...'), nl,
        player_move(MovesLeft, GameState, FinalGameState)
    ).



%-------------------------------------------CHOOSE_PIECE-----------------------------------------------%
%CASE- PLAYER 1
choose_piece(game_state(_, player1Info(_,Player1_Pieces_Available, _),_,_,_, 1, _ ), piece(1, PieceNbr)):-
    
    length(Player1_Pieces_Available,PiecesAvailable),

    write('[PLAYER 1]'),nl,
    write('======================================='), nl,
    write('|     == == Available pieces == ==    |'), nl,
    write('======================================='), nl,

    display_available_pieces(Player1_Pieces_Available),nl,
    
    write('======================================='), nl,nl,

    format('Enter piece number (1 to ~w): ', [PiecesAvailable]),
    repeat,
    read_number(PieceNbr), clear_buffer,
    (between(1, PiecesAvailable, PieceNbr) -> !, true ; write('Invalid input, try again'), nl,nl, fail).

%CASE PLAYER 2
choose_piece(game_state(_, _, player2Info(_,Player2_Pieces_Available, _),_,_, 2, _ ), piece(2, PieceNbr)):-
    length(Player2_Pieces_Available,PiecesAvailable),

    write('[PLAYER 2]'),nl,
    write('======================================='), nl,
    write('|     == == Available pieces == ==    |'), nl,
    write('======================================='), nl,

    display_available_pieces(Player2_Pieces_Available),nl,
    
    write('======================================='), nl,nl,

    format('Enter piece number (1 to ~w): ', [PiecesAvailable]),
    repeat,
    read_number(PieceNbr), clear_buffer,
    (between(1, PiecesAvailable, PieceNbr) -> !, true ; write('Invalid input, try again'), nl,nl, fail).
    


display_available_pieces([]) :- !.
display_available_pieces([Head | Tail]) :-
    format('      > Piece ~w       ', [Head]), nl,
    display_available_pieces(Tail).



%-------------------------------------------ENTER_MOVE-----------------------------------------------%

enter_move( game_state(_, _, _,_,_, _, boardInfo(Width,Height,_)), piece(Current_Player, PieceNbr), move(piece(Current_Player, PieceNbr), dest(Row, Collumn, Color))):-

    format('--> Piece Choosen: ~w - ~w       ', [Current_Player, PieceNbr]), nl,

    Adapted_Height is Height +2,
    write('======================================='), nl,
    write('|==   ==   Select target tile   ==   ==|'), nl,
     write('======================================='), nl,
    
    format('Enter row number (1-~w): ', [Adapted_Height]),nl,
    repeat,
    read_number(Row),clear_buffer,
    (between(1, Adapted_Height, Row) -> !, true ; write('Invalid input, try again'), nl,nl, fail),
   

    format('Enter collumn number (1-~w): ', [Width]),nl,  
    repeat,
    read_number(Collumn),clear_buffer,
    (between(1, Width, Collumn) -> !, true ; write('Invalid input, try again'), nl,nl, fail),


    write('Enter color {orange, blue, gray}: '),    
    read_color(Color),clear_buffer,

    write('======================================='), 
    
    
    nl,nl,nl.

%------------------------------------------- CHECK_MOVE -----------------------------------------------%
%PLAYER1
check_move(game_state(_, _, _,_,_, 1, boardInfo(Width,Height,CurrentBoard)), move(piece(1, Piece), dest(Row, Col, Color))):-   
    translate_row_input(Row, Height, RealRow), 
    is_move_possible(CurrentBoard, game_config(Width, Height), 1, 1-Piece, RealRow-Col-Color), !.
   
%PLAYER1 Base
check_move(game_state(_, _, _,_,_, 1, boardInfo(Width,Height,CurrentBoard)), move(piece(1, Piece), dest(Row, Col, Color))):-    
    translate_row_input(Row, Height, RealRow),
    is_move_possible_start(CurrentBoard, game_config(Width, Height), 1, 1-Piece, RealRow-Col-Color), !.

%PLAYER2 
check_move(game_state(_, _, _,_,_, 2, boardInfo(Width,Height,CurrentBoard)), move(piece(2, Piece), dest(Row, Col, Color))):-    
    translate_row_input(Row, Height, RealRow), 
    is_move_possible(CurrentBoard, game_config(Width, Height), 2, 2-Piece, RealRow-Col-Color), !.

%PLAYER2 BASE
check_move(game_state(_, _, _,_,_, 2, boardInfo(Width,Height,CurrentBoard)), move(piece(2, Piece), dest(Row, Col, Color))):-    
    translate_row_input(Row, Height, RealRow), 
    is_move_possible_start(CurrentBoard, game_config(Width, Height), 2, 2-Piece, RealRow-Col-Color), !.

%------------------------------------------- MOVE -----------------------------------------------%

%PLAYER 1
  move(
    game_state(Turn, player1Info(Player1_Type, P1_Available, P1_Pieces_Delivered),player2Info(Player2_Type,P2_Available, P2_Delivered), PiecesPerPlayer, PiecesNecessaryToWin, 1, boardInfo(Width, Height, CurrentBoard) ), 
    move(piece(Player, Piece), dest(Row, Col, Color)),
    game_state(Turn, player1Info(Player1_Type, New_P1PiecesAvailable, New_P1PiecesDelivered),player2Info(Player2_Type,P2_Available, P2_Delivered), PiecesPerPlayer, PiecesNecessaryToWin, 1, boardInfo(Width, Height, NewBoard) )):-
    
    translate_row_input(Row, Height, RealRow), %to fix coordintate backend discrepancies
  
    move_piece(CurrentBoard, game_config(Width, Height), Player, Player-Piece, RealRow-Col-Color,P1_Available,P1_Pieces_Delivered, NewBoard, New_P1PiecesAvailable, New_P1PiecesDelivered),

    display_current_board(Width,Height,NewBoard),nl, !.


%PLAYER 2
move(
    game_state(Turn,player1Info(Player1_Type, P1_Available, P1_Pieces_Delivered), player2Info(Player2_Type,P2_Available, P2_Delivered), PiecesPerPlayer, PiecesNecessaryToWin, 2, boardInfo(Width, Height, CurrentBoard) ), 
    move(piece(Player, Piece), dest(Row, Col, Color)), 
    game_state(Turn,player1Info(Player1_Type, P1_Available, P1_Pieces_Delivered), player2Info(Player2_Type, New_P2PiecesAvailable, New_P2PiecesDelivered), PiecesPerPlayer, PiecesNecessaryToWin, 2, boardInfo(Width, Height, NewBoard) )):-

    translate_row_input(Row, Height, RealRow), %to fix coordintate backend discrepancies
  
    move_piece(CurrentBoard, game_config(Width, Height), Player, Player-Piece, RealRow-Col-Color,P2_Available,P2_Delivered, NewBoard, New_P2PiecesAvailable,New_P2PiecesDelivered),

    display_current_board(Width,Height,NewBoard),nl, !.
