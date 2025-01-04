:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(random)).
:- consult(utils).


%----------------------------------------generate INITIAL BOARD----------------------------------------%
% Generate an initial board with randomized tiles based on the specified width and height

generate_initial_board(Width, Height, RandomizedBoard) :-
    findall(
        RowTiles,
        (between(1, Height, Row),
         findall(tile(Row, Col, [(g-black, 0-0), (o-orange, 0-0), ( b-blue, 0-0), ( ' '-yellow, 0-1)]),
                 between(1, Width, Col),
                 RowTiles)),
        DefaultBoard),
    maplist(maplist(randomize_tile_flowers), DefaultBoard, RandomizedBoard).


%----------------------------------------DISPLAY CURRENT BOARD----------------------------------------%

% Display the board
display_current_board(Width, Height, Board) :-
    
    draw_base(Width, 'ORANGE'),nl,
    display_board_rows(Height, Board),

    draw_padding(0),
    draw_bottom_borders(Width), nl,
    
    draw_padding(0),
    draw_X_coordinates(Width,0),nl,
    draw_base(Width, 'BLUE'),nl,nl.

display_board_rows(_,[]) :- !.
display_board_rows(Collumn_Counter,[Row | Rest]) :-
    draw_row(Row,Collumn_Counter),nl,

    NEW_Collumn_Counter is Collumn_Counter - 1,

    display_board_rows(NEW_Collumn_Counter,Rest).


% Draw the entire row in sections with '++' separators
draw_row(Tiles, Collumn_Counter) :-
    draw_padding(0),
    draw_top_borders(Tiles), nl,

    draw_padding(0),
    draw_flower_row1(Tiles), nl,

    draw_padding(1),
    draw_Y_coordinates(Collumn_Counter),
    draw_padding(2),
    draw_flower_tokens1(Tiles), nl,

    draw_padding(0),
    draw_flower_row2(Tiles), nl,

    draw_padding(0),
    draw_flower_tokens2(Tiles).


%------------------DRAW PADDING------------------%

draw_padding(0) :- write('      ').

draw_padding(1) :- write('    ').

draw_padding(2) :- write(' ').



%------------------DRAW PLAYER BASES------------------%
draw_base(Width, Color) :-
    write('       '),
    Half_Width is Width // 2,
    draw_base_aux(Half_Width),
    format('        ~w BASE      ', [Color]),
    draw_base_aux(Half_Width).

draw_base_aux(1).
draw_base_aux(Half_Width) :-
    write('/////////////'),
    NEW_Width is Half_Width - 1,
    draw_base_aux(NEW_Width).

%------------------DRAW COORDINATES------------------%
draw_Y_coordinates(Counter):-
    format('~d', [Counter]).
    

draw_X_coordinates(0,_).
draw_X_coordinates(N_Collumns,Counter) :-
    NEW_N_Collumns is N_Collumns - 1,
    NEW_Counter is Counter + 1,

    write('      '),
    format('~d', [NEW_Counter]),
    write('      '),
    
    draw_X_coordinates(NEW_N_Collumns,NEW_Counter).

%------------------DRAW BORDERS------------------%

draw_bottom_borders(0).
draw_bottom_borders(N_Collumns) :-
    NEW_N_Collumns is N_Collumns - 1,

    write('+-----+-----+'),

    draw_bottom_borders(NEW_N_Collumns).


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


%--------------DRAW top BORDERS----------------%
draw_top_borders([]).

draw_top_borders([_ | []]) :- 
    write('+-----+-----+').

draw_top_borders([_ | Rest]) :-
    write('+-----+-----+'),
    draw_top_borders(Rest).

% Display the token (player-pieceNumber)
display_token(0-0) :- write('---').
display_token(0-1) :- write('   ').
display_token(1-T) :- write(' '), display_color(T-blue), write(' ').
display_token(2-T) :- write(' '), display_color(T-orange), write(' ').

