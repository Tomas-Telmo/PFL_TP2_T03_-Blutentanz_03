:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(random)).


select_difficulty(1, none, none). % H/H

select_difficulty(2, none, Difficulty) :- % H/PC
    write('Enter difficulty for Computer: '), read(Difficulty).

select_difficulty(3, Difficulty, none) :- % PC/H
    write('Enter difficulty for Computer: '), read(Difficulty).

select_difficulty(4, Difficulty1, Difficulty2) :- % PC/PC
    write('Enter difficulty for Computer 1: '), read(Difficulty1),
    write('Enter difficulty for Computer 2: '), read(Difficulty2).


