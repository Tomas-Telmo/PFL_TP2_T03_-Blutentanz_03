:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(random)).

%select_difficulty(GameType, Difficulty_Bot1, Difficulty_Bot2,player1Type,player2Type) 

initial_player_settings(1, none, none, human, human). % H/H


initial_player_settings(2, none, Difficulty, human, Player2_Type) :- % H/PC
    
	write('==========================='), nl,
    write('|  SELECT BOT DIFFICULTY  |'), nl,
    write('==========================='), nl, 
    write('|  1. Dumb                 |'), nl,
    write('|  2. Expert               |' ), nl,
    write('==========================='), nl,nl,nl,nl,
	
    write('Enter difficulty for BOT: '), read(Difficulty),

    initial_player_settings_aux(2, none, Difficulty, human, Player2_Type).



initial_player_settings(3, Difficulty, none, Player1_Type, human) :- % PC/H
	write('==========================='), nl,
    write('|  SELECT BOT DIFFICULTY  |'), nl,
    write('==========================='), nl,
    write('|  1. Dumb                 |'), nl,
    write('|  2. Expert               |' ), nl,
    write('==========================='), nl,
	
	write('Enter difficulty for BOT: '), read(Difficulty),nl,nl,nl,

    initial_player_settings_aux(3, Difficulty, none ,Player1_Type, human).



initial_player_settings(4, Difficulty1, Difficulty2, Player1_Type, Player2_Type) :- % PC/PC
	
	write('==========================='), nl,
    write('|  SELECT BOT-1 DIFFICULTY |'), nl,
    write('==========================='), nl,
    write('|  1. Dumb                 |'), nl,
    write('|  2. Expert               |' ), nl,
    write('==========================='), nl,
	
    write('Enter difficulty for BOT-1: '), read(Difficulty1),


	write('==========================='), nl,
    write('|  SELECT BOT-2 DIFFICULTY |'), nl,
    write('==========================='), nl,
    write('|  1. Dumb                 |'), nl,
    write('|  2. Expert               |' ), nl,
    write('==========================='), nl,
	
    write('Enter difficulty for BOT-2: '), read(Difficulty2),nl,nl,nl,

    initial_player_settings_aux(4, Difficulty1, Difficulty2, Player1_Type, Player2_Type).


initial_player_settings_aux(2, none, 1, human, dumbBot).
initial_player_settings_aux(2, none, 2, human, expertBot).

initial_player_settings_aux(3, 1, none ,dumbBot, human).
initial_player_settings_aux(3, 2, none ,expertBot, human).

initial_player_settings_aux(4, 1, 1, dumbBot, dumbBot).
initial_player_settings_aux(4, 1, 2, dumbBot, expertBot).
initial_player_settings_aux(4, 2, 1, expertBot, dumbBot).
initial_player_settings_aux(4, 2, 2, expertBot, expertBot).