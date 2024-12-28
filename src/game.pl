:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(random)).
:- consult(board).

% Ola Sr telmo, viemos a descobrir que,no dia de hj, nao pagou a fatura da luz. Por este motivo foi condenado pelo grandioso tribunal dos bacalhaus vagabundos.
% Terá de pagar uma coima de 300 rissois de camarão ao delegado Marcelo Rebelo de Sousa delegado da turma, o qual estará a sua espera no local do bar do liceu, onde o bolo de marmore e muito famoso.
% Devera pagar tambem dois bolinhos de arroz á Dona Fatima da cantina, por consequente esperar na fila da reprografia com a muchila cheia de camaroes a depositar.
% Apos o pagamento da coima, devera dirigir-se ao bar do liceu, onde o delegado Marcelo Rebelo de Sousa, lhe entregara um bilhete para a festa de natal, onde podera comer os camaroes que lhe sobraram.

%----------------------------------------GAME STATE----------------------------------------%

% state(TurnNo, Player1Info, Player2Info, Board) represents the current state of the game.
% TurnNo is the current turn number.
% Player1Info and Player2Info contain information about the players, such as the number of remaining pieces to place.
% Board is the current state of the game board.
% state(TurnNo, Player1Info, Player2Info, Board) represents the current state of the game.
% TurnNo is the current turn number.
% Player1Info and Player2Info contains the number of remaining pieces to place on the board.
% Board is the current state of the game board.

%state(TurnNo, Player1Info, Player2Info, Board) :-
%state(TurnNo, Player1Info, Player2Info, Board).

% beginning of a game
% initial_state(Config, State) initializes the game state.
% Config is a configuration term containing the width and height of the board and the number of pieces per player.
% State is the initial state of the game, with the first turn, players having the specified number of pieces, and the initial board setup.

initial_state(config(Width, Height, PiecesPerPlayer), state(1, player(PiecesPerPlayer), player(PiecesPerPlayer), Board)) :-
	
	format('Pieces of player1: ~w\n', [PiecesPerPlayer]), 
	format('Pieces of player2: ~w\n', [PiecesPerPlayer]),
	
	display_initial_board(Width, Height, Board).




