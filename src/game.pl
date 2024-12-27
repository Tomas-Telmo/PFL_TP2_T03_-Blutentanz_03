
tile(Row, Col, Flowers, Token).


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
	tile(4, 4, [orange, gray, blue, none], empty),
].


%example state(1, player(5), player(5), Board). player(remaining pieces to place)
state(TurnNo, Player1Info, Player2Info, Board):-

%begining of a game
state(1, player1(5), player2(5), Board).

initial_state(config(Width, Height, PiecesPerPlayer), state(1, player(PiecesPerPlayer), player(PiecesPerPlayer), Board)) :-
    initial_board(Width, Height, Board).