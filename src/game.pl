:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(random)).
:- consult(board).
:- consult(playerSettings).

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
    read(GameType),
        
	initial_player_settings(GameType, _, _, Player1_Type, Player2_Type),

	write('======================================='), nl,
    write('|            GAME CONFIGS             |'), nl,
    write('======================================='), nl,

    write('       >Enter board width:           '),read(Width),nl,
    write('       >Enter board height:          '),read(Height),nl,
    write('       >Enter pieces per player:     '), read(PiecesPerPlayer),nl,nl,nl,nl,
    
    
    initial_state(config(Width, Height, PiecesPerPlayer, Player1_Type, Player2_Type), state(1, player(PiecesPerPlayer), player(PiecesPerPlayer), _ )).
    
    %game_cycle(GameState).
    


%----------------------------------------GAME STATE----------------------------------------%

% beginning of a game
% initial_state(Config, State) initializes the game state.

% Config is a configuration term containing the width and height of the board and the number of pieces per player.
% State is the initial state of the game, with the first turn, players having the specified number of pieces, and the initial board setup.

% state(TurnNo, Player1Info, Player2Info, Board) represents the current state of the game.
% TurnNo is the current turn number.
% Player1Info and Player2Info contain information about the players, such as the number of remaining pieces to place.
% Board is the current state of the game board.
% state(TurnNo, Player1Info, Player2Info, Board) represents the current state of the game.
% TurnNo is the current turn number.
% Player1Info and Player2Info contains the number of remaining pieces to place on the board.
% Board is the current state of the game board.


initial_state(config(Width, Height, PiecesPerPlayer, Player1_Type, Player2_Type), state(1, player(Player1_Pieces), player(Player2_Pieces), Board)) :-
	
    write('======================================='), nl,
    write('|            GAME STARTING...         |'), nl,
    write('======================================='), nl,
	format('Pieces per player: ~w\n', [PiecesPerPlayer]),nl,
    format('Board Width: ~w\n', [Width]),nl,
    format('Board Height: ~w\n', [Height]),nl,nl,nl,
    
    

    write('======================================='), nl,
    write('|               BEGIN!                 |'), nl,
    write('======================================='), nl,nl,
    format('Player 1: ~w\n', [Player1_Type]),nl,
    format('Pieces remaining: ~w\n', [Player1_Pieces]),nl,nl,
	
	format('Player 2: ~w\n', [Player2_Type]),nl,
	format('Pieces remaining: ~w\n', [Player2_Pieces]),nl,
	
	display_initial_board(Width, Height, Board).




