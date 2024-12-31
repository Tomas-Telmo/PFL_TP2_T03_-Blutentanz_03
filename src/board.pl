:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(random)).
:- consult(utils).


%----------------------------------------DISPLAY BOARD----------------------------------------%
% Display the starter board starting at row 1
display_initial_board(Width, Height, Board) :-
    initial_board(Width, Height, Board),
    display_dummy(Board).

% Generate an initial board with randomized tiles based on the specified width and height
initial_board(Width, Height, RandomizedBoard) :-
    findall(
        RowTiles,
        (between(1, Height, Row),
         findall(tile(Row, Col, [(o-orange, 0-0), (g-black, 0-0), ( b-blue, 0-0), ( ' '-black, 0-1)]),
                 between(1, Width, Col),
                 RowTiles)),
        DefaultBoard),
    maplist(maplist(randomize_tile_flowers), DefaultBoard, RandomizedBoard).


% Display the board
display_dummy(Board) :-
    draw_top_borders(Board,0), nl,
    display_dummy_rows(Board).

display_dummy_rows([]) :- !.
display_dummy_rows([Row | Rest]) :-
    draw_row(Row), 
    nl,
    display_dummy_rows(Rest).


% Draw the entire row in sections with '++' separators
draw_row(Tiles) :-
    draw_flower_row1(Tiles), nl,

    draw_flower_tokens1(Tiles), nl,

    draw_flower_row2(Tiles), nl,

    draw_flower_tokens2(Tiles), nl,

    draw_bottom_borders(Tiles).

%------------------DRAW BORDERS------------------%
% Draw the top borders for the tiles with '++' as separators
draw_top_borders([]).

draw_top_borders([_ | []], Counter) :- 
    Counter1 is Counter + 1,

    write('+---- '),
   format('~d', [Counter1]),
    write(' ----+').

draw_top_borders([_ | Rest],Counter) :-
    Counter1 is Counter + 1,

    write('+---- '),
   format('~d', [Counter1]),
    write(' ----+'),
    
    draw_top_borders(Rest,Counter1).

%------------------DRAW FLOWERS------------------%

% Draw row1 flowers
draw_flower_row1([]).

draw_flower_row1([tile(_, _, [( Color1, _), ( Color2, _), _, _]) | []]) :-
    write('|  '), display_color(Color1), write('  |  '), display_color(Color2), write('  |').

draw_flower_row1([tile(_, _, [( Color1, _), ( Color2, _), _, _]) | Rest]) :-
    write('|  '), display_color(Color1), write('  |  '), display_color(Color2), write('  |'),
    draw_flower_row1(Rest).


% Draw row1 tokens
draw_flower_tokens1([]).

draw_flower_tokens1([tile(_, _, [( _, Token1), ( _, Token2), _, _]) | []]) :-
    write('| '), display_token(Token1), write(' | '), display_token(Token2), write(' |').

draw_flower_tokens1([tile(_, _, [( _, Token1), ( _, Token2), _, _]) | Rest]) :-
    write('| '), display_token(Token1), write(' | '), display_token(Token2), write(' |'),
    draw_flower_tokens1(Rest).


% Draw row2 flowers
draw_flower_row2([]).

draw_flower_row2([tile(_, _, [_, _, ( Color3, _), ( Color4, _)]) | []]) :-
    write('|  '), display_color(Color3), write('  |  '), display_color(Color4), write('  |').

draw_flower_row2([tile(_, _, [_, _, ( Color3, _), ( Color4, _)]) | Rest]) :-
    write('|  '), display_color(Color3), write('  |  '), display_color(Color4), write('  |'),
    draw_flower_row2(Rest).

% Draw row2 tokens
draw_flower_tokens2([]).

draw_flower_tokens2([tile(_, _, [_, _, ( _, Token3), ( _, Token4)]) | []]) :-
    write('| '), display_token(Token3), write(' | '), display_token(Token4), write(' |').

draw_flower_tokens2([tile(_, _, [_, _, ( _, Token3), ( _, Token4)]) | Rest]) :-
    write('| '), display_token(Token3), write(' | '), display_token(Token4), write(' |'),
    draw_flower_tokens2(Rest).


%--------------DRAW BOTTOM BORDERS----------------%
draw_bottom_borders([]).

draw_bottom_borders([_ | []]) :- 
    write('+-----+-----+').

draw_bottom_borders([_ | Rest]) :-
    write('+-----+-----+'),
    draw_bottom_borders(Rest).

% Display the token (player-pieceNumber)
display_token(0-0) :- write('---').
display_token(0-1) :- write('   ').
display_token(1-T) :- write(' '), display_color(T-blue), write(' ').
display_token(2-T) :- write(' '), display_color(T-orange), write(' ').



