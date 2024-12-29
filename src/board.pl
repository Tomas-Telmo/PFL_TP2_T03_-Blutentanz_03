:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(random)).
:- consult(utils).


%----------------------------------------DISPLAY BOARD----------------------------------------%

% Display the starter board starting at row 1
display_initial_board(Width, Height, Board) :-
	initial_board(Width, Height, Board),
	display_board(Board, 1, Height).

% Generate a board with tiles based on the specified width and height
initial_board(Width, Height, Board) :-
	findall(tile(Row, Col, [o-orange, g-green, b-blue, x-black], '---'), 
			(between(1, Height, Row), between(1, Width, Col)), 
			DummyBord),
    maplist(randomize_tile_colors, DummyBord, Board).


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
    write('| '), display_color(F1), write(' '), display_color(F2), write(' |'),
    draw_flower_row1(Rest).

% Draw the second row of flowers (bottom flowers) for all tiles
draw_flower_row2([]).
draw_flower_row2([tile(_, _, [_, _, F3, F4], _) | Rest]) :-
    write('| '), display_color(F3), write(' '), display_color(F4), write(' |'),
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



%--------------ROTATE ROWS OR COLUMNS BY 90------------%
rotater([], []).
rotater([tile(Row, Col, Colors, Symbol) | Rest],[tile(Row, Col, RotatedColors, Symbol) | RotatedRest]) :-
    rotate_90_right(Colors, RotatedColors),
    rotater(Rest, RotatedRest).          

%-------------CHECK ROW EXISTS AND ROTATE ROW-------%
rotate_specified_row(Board, RowNum, NewBoard) :-
    nth1(RowNum, Board, Row, RestBoard),
    rotater(Row, RotatedRow),
    nth1(RowNum, NewBoard, RotatedRow, RestBoard).

%------------CHECK COLUMN EXISTS AND ROTATE COLUMN----%
rotate_specified_column(Board, ColNum, NewBoard) :-
    transpose(Board, TransposedBoard),            % Transpose the board to turn columns into rows
    nth1(ColNum, TransposedBoard, Column, Rest), % Extract the column (now a row) and the rest of the transposed board
    rotater(Column, RotatedColumn),              % Rotate the extracted column
    nth1(ColNum, NewTransposed, RotatedColumn, Rest), % Insert the rotated column back
    transpose(NewTransposed, NewBoard).          % Transpose back to get the original structure

%------------------------------------------------------------------------------------------%

sample_board([
    [tile(1, 1, [o, g, b, x], '---'), tile(1, 2, [o, b, g, x], '---')],
    [tile(2, 1, [b, x, g, o], '---'), tile(2, 2, [x, g, o, b], '---')]
]).

