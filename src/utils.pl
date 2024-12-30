:- use_module(library(lists)).
:- use_module(library(random)).

% Map color names to ANSI escape sequences as strings
color_code(black, "\e[30m"). 
color_code(orange, "\e[38;2;255;165;0m").  
color_code(white, "\e[37m").  
color_code(red, "\e[31m").       
color_code(green, "\e[32m").     
color_code(yellow, "\e[33m").    
color_code(blue, "\e[34m").      
color_code(magenta, "\e[35m").   
color_code(cyan, "\e[36m").      
color_code(white, "\e[37m").     
color_code(reset, "\e[0m").      

% Predicate to print colored text
display_color(Letter-Color):-
    print_color(Letter, Color).
    
% Aux to Predicate to print colored text
print_color(Text, Color) :-
    color_code(Color, Code),       
    color_code(reset, Reset),      
    format("~s~s~s", [Code, Text, Reset]). 

  
%funçao para rodar a lista 90 graus para a direita como so temos uma 2x2 podemos fazer a rotaçao manualmente.
rotate_90_right([A, B, C, D], [C, A, D, B]).

% Rotate the list 90 degrees N times
rotate_n_times(List, 0, List). 
rotate_n_times(List, N, Result) :-
    N > 0,
    rotate_90_right(List, Rotated),
    N1 is N - 1,
    rotate_n_times(Rotated, N1, Result).

% Randomly rotate the list 0 to 3 times
random_rotate(List, Rotated) :-
    random(X),                 
    N is floor(X * 4),         
    rotate_n_times(List, N, Rotated).

% randomize each tile, functuin for the begining of the game
randomize_tile_flowers(tile(Row, Col, Flowers), tile(Row, Col, Rotatedflowers)) :-
    random_rotate(Flowers, Rotatedflowers).


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
