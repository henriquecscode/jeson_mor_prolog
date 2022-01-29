:- use_module(library(random)).
:- ensure_loaded('./view.pl').


% Prints a line with two specified symbols sperated by spaces (Padding number of times)
print_blank_line(Symbol, Padding) :-
    put_char(Symbol), 
    print_n(' ', Padding), 
    put_char(Symbol).

% Prints a text line with encapsulated by two symbols and with padding inbetween
print_text_padding(Text, Symbol, Padding) :- 
    put_char(Symbol),
    print_n(' ', Padding),
    write(Text),
    print_n(' ', Padding),
    put_char(Symbol).

% Prints a char array recursively into the output stream
print_str([]).
print_str([X | R]) :- put_code(X), print_str(R).

% Retrieves chars from the Input Array and parses through them recursively
parse_chars(Input):-
        get_char(user_input, C),
        parse_char(C, Input).

% Parses a given char converting its ASCII number into an atom
parse_char('\n', ''):- !.
parse_char(Code, I):-
        get_char(C),
        atom_concat(Code, C, I).

% Asks for a nl input to go over to another action
enter:-
        write('Press enter to continue.'), nl,
        parse_chars(_Input).

% Parses an int from a ASCII numbered array
% If no int is selected (as this function is only used in the board configuration), 9, the default board size is returned
parse_int(L, N) :- parse_int(L, 0, N).
parse_int([], 0, 9). % default board size 
parse_int([32 | _], Acc, Acc). %reached a space, return the accumulated number
parse_int([], Acc, Acc) :- !. %empty lists returns
parse_int([D | T], Acc, Result) :- 
    N is (D - 48), % subtract the 0 ASCII value
    N < 10, %char is a number
    NewN is Acc * 10 + N, %the previous number plus a new decimal digit
    parse_int(T, NewN, R),
    Result is R.

% Executes parse_int and returns its value
parse_number(L, Number) :-
    parse_int(L, 0, Res), 
    Number is Res.


% Board configuration, repeats the loop until the user enters or inputs a valid board size
get_board_size(N) :-
    repeat, %repeats until success
        write('Please enter the board size: '), 
        read_line(L),
        parse_number(L, Int), 
        Int mod 2 =\= 0, % has center cavity, %number is odd
        Int < 26, Int >= 5, %falls into the limits
        !,
    N is Int.

% Game type configuration
% Prompts the user, asking which type of game he/she would like to play
% If AI, prompt for ai level and precedence
% If human vs human, ask for precedence or "flip a coin"
get_game_players(IsAi, P):-
    playing_ai(R),
    (
        R = y,
        select_ai_level(IsAi),
        select_ai_precedence(P)
    )
    ;
    (
        player_menu,
        IsAi = n,
        P = p1
    )
    .


% PLAYER COSTUMIZATION

parse_bool_answer([121], y) :- !.
parse_bool_answer([110], n) :- !.
parse_bool_answer(_, _) :- fail.

% Validates a y or n answer 
validate_answer(Text, R) :- 
    repeat,
        write(Text), 
        read_line(L),
        parse_bool_answer(L, Response), 
        !,
    R = Response.

playing_ai(R) :- 
    validate_answer('Are you playing the AI system? ', R).

print_ai_levels :- 
    write('Level 0: Random moves'),
    write('Level 1: Best current moves'), nl.

% Ask the user for a numbered input, validating and returning the selected AI level 
select_ai_level(N) :- 
    repeat,
        write('Enter the level: 0 or 1? '), 
        read_line(L),
        length(L, Length),
        Length > 0,
        parse_number(L, Level), 
        Level >= 0, Level < 2, 
        !,
    (
        (Level = 0, N = ai1); % assigns the correct ai level 
        (Level = 1, N = ai2)
    ).
    
select_ai_precedence(F):-
    validate_answer('Is the AI going first? ', R),
    (
        (R = y, F = p1, write('AI will be playing as the whites'));
        (R = n, F = p2, write('AI will be playing as the blacks'))
    ), nl.

playing_ai_menu(Answer, Level) :- 
    playing_ai(A), 
    (A = n, Answer = A);
    (A = y, select_ai_level(Level)).

player_concede(Name, R) :- 
    atom_concat(Name, ' do you concede your turn [y/n]? ', T), 
    validate_answer(T, R).

% Assigns the player name who will start the game, according to the concede turn menu configurations
select_first_player(P1,_, n, y, P1) :- !.
select_first_player(_, P2, y, n, P2) :- !.
select_first_player(P1, _, _, _, P1) :- 
    random(0, 2, R), % gives a 50/50 in case both user do not concede their turn
    R =:= 0, !.
select_first_player(_, P2, _, _, P2).

% Menu that takes care of asking and selecting the first player
first_player_decision(P1, P2, FP) :- 
    player_concede(P1, R1),
    player_concede(P2, R2), 
    select_first_player(P1, P2, R1, R2, FP).

% Converts an ASCII numbered array into a char atom list
% Used atom_concat and char_code to parse the char and converting it (predicates in the standard)

convert_chars([], Acc, Acc) :- !.
convert_chars([H | T], Acc, F) :- 
    char_code(C, H),
    atom_concat(Acc, C, NewAcc), 
    convert_chars(T, NewAcc, F).

% Player configuration menu

player_menu :-
    write('Enter player one name: '),
    get_player(L1),
    write('Enter player two name: '),
    get_player(L2),
    first_player_decision(L1, L2, FP), 
    atom_concat(FP, ' you have the white pieces, so you have the first turn.', T),  
    write(T), 
    nl,
    enter.


% Allows for an help prompt, in case the user forgets the game rules when making a move

help([104,101,108,112]) :- 
    write('Dont forget the objective, capture all pieces or go to the center cavity'), 
    nl, 
    fail.
help(_) :- !.

% Parses a char line (allowing for extra input spaces) that returns the column of the coordinate

parse_col([32 | T], Col, RestOfInput) :- parse_col(T, Col, RestOfInput), !.
parse_col([C | T], Col, T) :- 
    (C > 64, C < 91, 
    Col is C - 65) ; 
    (C > 96, C < 123, 
    Col is C - 97), !.
parse_col(_, _, _) :- fail.

% Parses a numbered line (allowing for extra input spaces) that returns the row of the coordinate
% Fails if the number is lower than 0

parse_row([32 | T], Row) :- parse_row(T, Row), !.
parse_row(L, Row) :- 
    parse_int(L, RT), 
    RT > 0, 
    Row is RT.

% From a splitted line returns the Col and Row direct array numbers. 
parse_move(L, Col, Row) :- 
    help(L), 
    parse_col(L, Col, RestOfInput), 
    parse_row(RestOfInput, Row), 
    !.

% Concates the given text with the specified players color

concat_color(p1, T, FT) :- atom_concat('White: ', T, FT).
concat_color(p2, T, FT) :- atom_concat('Black: ', T, FT).
concat_color(ai, T, FT) :- atom_concat('AI: ', T, FT).

% Crites text with the players identification  
write_action(P, T) :- 
    concat_color(P, T, FT), 
    write(FT).

% Reads a non empty player

get_player(L):-
    repeat,
        read_line(L1),
        length(L1, Length),
        Length > 0,
        atom_codes(L, L1),
        !.

% Using the previously mentioned predicates returns the correct array accessing coordinates (start and end)

get_move(P, L1, C1, L2, C2) :- 
    repeat,
        write_action(P,'Please enter your move: <from col><from line>-<to col><to line>'), 
        read_line(L),
        help(L), 
        parse_hifen(L, Esq, Dir), 
        parse_move(Esq, ColF, RowF), 
        parse_move(Dir, ColT, RowT),
        !,
    L1 is RowF -1, 
    C1 is (ColF), 
    L2 is RowT - 1, 
    C2 is (ColT).

% Splits the read line into left and right sections (Start coord and End coord, sperated by '-')
parse_hifen(L, Esq, Dir) :- 
    parse_hifen(L, [], Esq, Dir).

parse_hifen([45 | T], Acc, Acc, T).
parse_hifen([C | T], Acc, Esq, Dir) :- 
    append(Acc, [C], Acc1), 
    parse_hifen(T, Acc1, Esq, Dir).