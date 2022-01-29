% :- ensure_loaded('./state_conversions.pl'). 
:- use_module(library(between)).

% Prints n symbols S
print_n(_, 0):- !.
print_n(S, N) :-
    N > 0,
    write(S),
    N1 is N - 1,
    print_n(S, N1).

subtract(X, X1):- X1 is X - 1.
subtract(X, S, X1):- X1 is X - S.

% Displays the GameState
display_game(GameState):-
    convert_game_state_to_var(GameState, B, _, _, _),
    length(B,N),
    L is N + 2,
    S is floor(log(10, N))+1,
    write(' '),
    print_n(' ', S),
    display_columns(N),
    nl,
    print_n(' ', S),
    print_n('-', L),
    nl,
    display_board_lines(B, N, S, 0),
    print_n(' ', S),
    print_n('-', L),
    nl.

% Displays the alphatbetical columns
display_columns(L):-
    L1 is L + 96,
    between(97, L1, X),
    char_code(C, X),
    write(C),
    fail;
    true. 

% Displays all the lines of the board, including the numerical row index
display_board_lines(_, L, _, L).
display_board_lines(B, L, S, I):-
    S1 is S - (floor(log(10,I+1))+1),
    print_n(' ', S1),
    X is I + 1,
    write(X),
    nth0(I, B, Line),
    display_board_line(Line),
    I1 is I + 1,
    display_board_lines(B,L,S,I1).

% Displays a line of the board
display_board_line(L):-
    write('|'),
    maplist(display_board_symbol, L),
    write('|'),
    nl.

% Displays an atom as an individual piece of the board
display_board_symbol(e):- write(' ').
display_board_symbol(w):- write('w').
display_board_symbol(b):- write('b').
display_board_symbol(x):- write('x').
