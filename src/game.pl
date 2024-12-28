:- use_module(library(lists)).
:- use_module(library(between)).

:- use_module(library(random)).

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

%----------------------------------------DISPLAY BOARD----------------------------------------%

% Display the starter board starting at row 1
display_initial_board(Width, Height, Board) :-
	initial_board(Width, Height, Board),
	display_board(Board, 1, Height).

% Generate a board with tiles based on the specified width and height
initial_board(Width, Height, Board) :-
	findall(tile(Row, Col, [o, g, b, x], '---'), 
			(between(1, Height, Row), between(1, Width, Col)), 
			Board).

% Base case: no more rows to display
display_board(_, Row, Height) :-
	Row > Height, !.
display_board(Board, Row, Height) :-
	display_row(Board, Row),  
	nl,                       
	NextRow is Row + 1,       
	display_board(Board, NextRow, Height).

% Display a single row
display_row(Board, Row) :-
    findall(tile(Row, Col, Flowers, Token), member(tile(Row, Col, Flowers, Token), Board), Tiles),
    draw_row(Tiles).

% Draw the entire row
draw_row(Tiles) :-
    draw_top_borders(Tiles), % Top border for all tiles
    nl,
    draw_flower_row1(Tiles), % First row of flowers for all tiles
    nl,
    draw_flower_row2(Tiles), % Second row of flowers for all tiles
    nl,
    draw_tokens(Tiles),      % Tokens for all tiles
    nl,
    draw_bottom_borders(Tiles), % Bottom border for all tiles
    nl.

%------------------DRAW BORDERS------------------%
% Draw the top borders for the tiles
draw_top_borders([]).
draw_top_borders([_ | Rest]) :-
    write('+-----+'),
    draw_top_borders(Rest).


%------------------DRAW FLOWERS------------------%
% Draw the first row of flowers (top flowers) for all tiles
draw_flower_row1([]).
draw_flower_row1([tile(_, _, [F1, F2, _, _], _) | Rest]) :-
    write('| '), write(F1), write(' '), write(F2), write(' |'),
    draw_flower_row1(Rest).

% Draw the second row of flowers (bottom flowers) for all tiles
draw_flower_row2([]).
draw_flower_row2([tile(_, _, [_, _, F3, F4], _) | Rest]) :-
    write('| '), write(F3), write(' '), write(F4), write(' |'),
    draw_flower_row2(Rest).


%------------------DRAW TOKENS------------------%
% Draw tokens (empty or player tokens)
draw_tokens([]).
draw_tokens([tile(_, _, _, Token) | Rest]) :-
    write('| '), write(Token), write(' |'),
    draw_tokens(Rest).


%--------------DRAW BOTTOM BORDERS----------------%
% Draw the bottom borders for the tiles
draw_bottom_borders([]).
draw_bottom_borders([_ | Rest]) :-
    write('+-----+'),
    draw_bottom_borders(Rest).

%------------------------------------------------------------------------------------------%



