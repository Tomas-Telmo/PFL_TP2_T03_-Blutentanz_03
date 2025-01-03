:- use_module(library(lists)).
:- use_module(library(random)).


% read_number(-X)
% Reads the first digits of a number. Throws the rest of the input away.
read_number(X):-
    read_number_aux(0,false,X).

% read_number_aux(+Acc,+HasAtLeastOneDigit,-X)
read_number_aux(Acc,_,X):- 
    peek_code(C),
    C >= 48,
    C =< 57,
    !, % used to ensure read_number is determinate
    get_code(_),
    Acc1 is 10*Acc + (C - 48),
    read_number_aux(Acc1,true,X).
    
read_number_aux(X,true,X).

% ?- repeat, read_number(X), clear_buffer, write(X), nl, fail.

% clear_buffer.
% Clears the input buffer.
clear_buffer:-
    repeat,
    get_char(C),
    C = '\n',
    !. % to avoid the true = ? on Sicstus 4.8.0+, works without the ! on older versions




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
    format("~s~w~s", [Code, Text, Reset]).


  
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
rotater([tile(Row, Col, Flowers) | Rest],[tile(Row, Col, RotatedFlowers) | RotatedRest]) :-
    rotate_90_right(Flowers, RotatedFlowers),
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


board([
    [tile(1, 1, [(g-black, 0-0), (o-orange, 0-0), (b-blue, 0-0), (' '-yellow, 0-1)]),
     tile(1, 2, [(g-black, 1-2),(o-orange, 0-0), (b-blue, 0-0), (' '-yellow, 0-1)])],
    [tile(2, 1, [(g-black, 0-0),(o-orange, 0-0), (b-blue, 0-0), (' '-yellow, 0-1)]),
     tile(2, 2, [(g-black, 0-0), (o-orange, 0-0) ,(b-blue, 0-0), (' '-yellow, 0-1)])]
]).
