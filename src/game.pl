:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(random)).
:- consult(board).
:- consult(playerSettings).

%----------------------------------------START GAME----------------------------------------%




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


initial_state(config(Width, Height, PiecesPerPlayer), state(1, player(Player1_Type, Player1_Pieces), player(Player2_Type, Player2_Pieces), Board)) :-
	
	format('Player 1: ~w\n', [Player1_Type]),
	format('Pieces of player1: ~w\n', [Player1_Pieces]), 
	
	format('Player 2: ~w\n', [Player2_Type]),
	format('Pieces of player2: ~w\n', [Player2_Pieces]),
	
	display_initial_board(Width, Height, Board).




