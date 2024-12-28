% Ola Sr telmo, viemos a descobrir que,no dia de hj, nao pagou a fatura da luz. Por este motivo foi condenado pelo grandioso tribunal dos bacalhaus vagabundos.
% Terá de pagar uma coima de 300 rissois de camarão ao delegado Marcelo Rebelo de Sousa delegado da turma, o qual estará a sua espera no local do bar do liceu, onde o bolo de marmore e muito famoso.
% Devera pagar tambem dois bolinhos de arroz á Dona Fatima da cantina, por consequente esperar na fila da reprografia com a muchila cheia de camaroes a depositar.
% Apos o pagamento da coima, devera dirigir-se ao bar do liceu, onde o delegado Marcelo Rebelo de Sousa, lhe entregara um bilhete para a festa de natal, onde podera comer os camaroes que lhe sobraram.

% tile(Row, Col, Flowers, Token) represents a tile on the board.
% tile(1, 2, [orange, gray, blue, none], empty) represents a tile at row 1, column 2, with orange, gray, and blue flowers, and no token.
% Token is the player on the tile. It can be empty, player1, or player2.
initial_board(Board) :-
	Board = [
		tile(1, 1, [orange, gray, blue, none], empty),
		tile(1, 2, [orange, gray, blue, none], empty),
		tile(1, 3, [orange, gray, blue, none], empty),
		tile(1, 4, [orange, gray, blue, none], empty),
		tile(2, 1, [orange, gray, blue, none], empty),
		tile(2, 2, [orange, gray, blue, none], empty),
		tile(2, 3, [orange, gray, blue, none], empty),
		tile(2, 4, [orange, gray, blue, none], empty),
		tile(3, 1, [orange, gray, blue, none], empty),
		tile(3, 2, [orange, gray, blue, none], empty),
		tile(3, 3, [orange, gray, blue, none], empty),
		tile(3, 4, [orange, gray, blue, none], empty),
		tile(4, 1, [orange, gray, blue, none], empty),
		tile(4, 2, [orange, gray, blue, none], empty),
		tile(4, 3, [orange, gray, blue, none], empty),
		tile(4, 4, [orange, gray, blue, none], empty)
	].


% display_board(+Board)
display_board(Board) :-
	initial_board(Board),
	display_board(Board, 1).

% display_board(+Board, +Row)
% Base case: no more rows to display.
display_board(_, 5) :- !.
display_board(Board, Row) :-
	display_row(Board, Row),  % Display the current row.
	nl,                       % Newline after the row.
	NextRow is Row + 1,       % Move to the next row.
	display_board(Board, NextRow).  % Recursively display the next rows.

% display_row(+Board, +Row)
% Finds all tiles in the board and starts drawing them.
display_row(Board, Row) :-
	findall(tile(Row, Col, Flowers, Token), member(tile(Row, Col, Flowers, Token), Board), Tiles),
	draw_row(Tiles).

draw_row([]) :- !.  % Base case: no tiles left.
draw_row(Tiles) :-
	draw_top_borders(Tiles),
	nl,
	draw_flowers(Tiles),
	nl,
	draw_tokens(Tiles),
	nl,
	draw_bottom_borders(Tiles),
	nl.

draw_top_borders([]).
draw_top_borders([_ | Rest]) :-
	write(' +------+ '),
	draw_top_borders(Rest).



draw_flowers([]).
draw_flowers([tile(_, _, Flowers, _) | Rest]) :-
	draw_individual_flowers(Flowers),
	draw_flowers(Rest).

draw_individual_flowers([F1, F2, F3, F4]) :-
	write(' | '), write(F1), write(' '), write(F2), write(' | '), nl,
	write(' | '), write(F3), write(' '), write(F4), write(' | ').

draw_individual_flowers([]) :- nl.



draw_tokens([]).
draw_tokens([tile(_, _, _, Token) | Rest]) :-
	write(' | '), write(Token), write('  | '),
	draw_tokens(Rest).

draw_bottom_borders([]).
draw_bottom_borders([_ | Rest]) :-
	write(' +------+ '),
	draw_bottom_borders(Rest).




















% state(TurnNo, Player1Info, Player2Info, Board) represents the current state of the game.
% TurnNo is the current turn number.
% Player1Info and Player2Info contain information about the players, such as the number of remaining pieces to place.
% Board is the current state of the game board.
% state(TurnNo, Player1Info, Player2Info, Board) represents the current state of the game.
% TurnNo is the current turn number.
% Player1Info and Player2Info contain information about the players, such as the number of remaining pieces to place.
% Board is the current state of the game board.

%state(TurnNo, Player1Info, Player2Info, Board) :-
%state(TurnNo, Player1Info, Player2Info, Board).


% beginning of a game
%state(1, player(5), player(5), Board).


% initial_state(Config, State) initializes the game state.
% Config is a configuration term containing the width and height of the board and the number of pieces per player.
% State is the initial state of the game, with the first turn, players having the specified number of pieces, and the initial board setup.
%initial_state(config(Width, Height, PiecesPerPlayer), state(1, player(PiecesPerPlayer), player(PiecesPerPlayer), Board)) :-

