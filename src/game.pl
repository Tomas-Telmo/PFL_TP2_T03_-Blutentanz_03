:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(random)).


:- consult(board).
:- consult(playerSettings).
:- consult(utils).
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
    nl,
    write('       >Enter board height:          '), read_number(Height),clear_buffer, nl,
    write('       >Enter pieces per player:     '), read_number(PiecesUnplaced),clear_buffer,nl,
    write('      >Enter pieces necessary to win:     '), read_number(PiecesNecessaryToWin),clear_buffer,nl,nl,nl,nl,
    
        
   


    initial_state(game_config(Width, Height), game_state(1, player1Info(Player1_Type,PiecesUnplaced, 0), player2Info(Player2_Type,PiecesUnplaced, 0),PiecesNecessaryToWin, player1, Board )),

    
    
    game_loop(game_state(1, player1Info(Player1_Type,PiecesUnplaced, 0), player2Info(Player2_Type,PiecesUnplaced, 0), PiecesNecessaryToWin, 1, Board )). 
    



    

%----------------------------------------GAME STATE----------------------------------------%

% beginning of a game
% initial_state(Config, State) initializes the game state.

% Config is a configuration term containing the width and height of the board and the number of pieces per player.
% State is the initial state of the game, with the first turn, players having the specified number of pieces, and the initial board setup.

 %initial_state(Config, Game_State),
        %config(Width, Height)
        %game_state(Round, Player1_Info, Player2_Info, PiecesNecessaryToWin, CurrentPlayer ,Board)),

    %game_state(ROUND, (Player1_Type,Player1_PiecesUnplaced, PLayer1_PiecesDelivered),(Player2_Type,Player2_PiecesUnplaced, PLayer2_PiecesDelivered),PiecesNecessaryToWin,CurrentPlayer, Board )),


  initial_state(game_config(Width, Height), game_state(Round, player1Info(Player1_Type,Player1_PiecesUnplaced, 0), player2Info(Player2_Type,_, 0),PiecesNecessaryToWin, CurrentPlayer, Board )) :-
	
    write('======================================='), nl,
    write('|            GAME SETTINGS...         |'), nl,
    write('======================================='), nl,
    format('Board Width: ~w\n', [Width]),nl,
    format('Board Height: ~w\n', [Height]),nl,
    format('Player 1: ~w\n', [Player1_Type]),nl,
    format('Player 2: ~w\n', [Player2_Type]),nl,
    format('Pieces necessary to win: ~w\n', [PiecesNecessaryToWin]),nl,
    write('======================================='), nl,nl,nl,
    

	display_initial_board(Width, Height, Board),


    write('======================================='), nl,
    format('|               ROUND ~w               |', [Round]),nl,
    write('======================================='), nl,

    format('------> player~w:                 ', [CurrentPlayer]),nl,

    format('Pieces remaining: ~w\n', [Player1_PiecesUnplaced]),nl,nl.
	
	
	%format('Pieces remaining: ~w\n', [Player2_PiecesUnplaced]),nl,nl,nl,
	

%----------------------------------------GAME LOOP----------------------------------------%
game_loop(GameState):-
    %game_over(+GameState, -Winner) fails here if game is not over, goes to next clause
    game_over(GameState, Winner), 

    %show_winner(+Winner) displays the winner
    show_winner(Winner), !. 

game_loop( game_state(Round, player1Info(Player1_Type,Player1_PiecesUnplaced, 0), player2Info(Player2_Type,Player2_PiecesUnplaced, 0),PiecesNecessaryToWin, CurrentPlayer, Board )):-
    %display_game(+GameState),
    display_game(GameState).
    
    %choose_move(CurrentBoard, X-Y),
   
   % piece(CurrentPlayer, Piece), put_piece(CurrentBoard, X-Y, Piece, NewBoard),
   
    %switch_player(CurrentPlayer, NewPlayer),
   
    %game_loop(game_state(Round, player1Info(Player1_Type,Player1_PiecesUnplaced, 0), player2Info(Player2_Type,Player2_PiecesUnplaced, 0),PiecesNecessaryToWin, CurrentPlayer, Board )).

%---------------------------------------GAME_OVER---------------------------------------------------%

game_over( game_state(_, player1Info(_,_, PiecesNecessaryToWin), _ ,PiecesNecessaryToWin, _, _ ), 1). % player1 wins
game_over( game_state(_, _, player2Info(_,_, PiecesNecessaryToWin) ,PiecesNecessaryToWin, _, _ ), 2).% player2 wins

%---------------------------------------SHOW_WINNER---------------------------------------------------%
show_winner(Winner):-
    write('======================================='), nl,
    write('|               GAME OVER             |'), nl,
    write('======================================='), nl,
    format('------> player~w wins!                 ', [Winner]),nl,nl,nl,nl.


%-------------------------------------------DISPLAY_GAME-----------------------------------------------%
display_game(game_state(Round, player1Info(Player1_Type,Player1_PiecesUnplaced, 0), player2Info(Player2_Type,Player2_PiecesUnplaced, 0),PiecesNecessaryToWin, CurrentPlayer, Board )):-

    write('======================================='), nl,
    format('|               ROUND ~w               |', [Round]),nl,
    write('======================================='), nl,

    format('------> player~w:                 ', [CurrentPlayer]),nl,

    format('Pieces remaining: ~w\n', [Player1_PiecesUnplaced]),nl,nl.

    