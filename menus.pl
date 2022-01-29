:- ensure_loaded('./input.pl').


% Welcome to Jeson Mor 
print_welcome_banner  :-
    print_n('X', 25), nl,
    print_blank_line('X', 23), nl,
    print_text_padding('Welcome to ', 'X', 6), nl,
    print_text_padding('  Jeson Mor', 'X', 6), nl,
    print_blank_line('X', 23), nl,
    print_n('X', 25), nl.


% Rules Banner
print_rules :-
    print_n('X', 72), nl, 
    print_text_padding('Pieces that move as the Chess Knight (in an L shape).', ' ', 2), nl,
    print_text_padding('Winning is achieved by:', ' ', 2), nl,
    print_text_padding('   - Moving one of the horses to the middle, and then outside the cell', ' ', 2), nl,
    print_text_padding('   - Capturing all enimies pieces', ' ', 2), nl,
    print_n('X', 72), nl.


% Prints the winner
print_winner(Winner):-
    write_action(Winner, ' won the game').

% Prints the initial menu
start_menu :-
    print_welcome_banner, 
    enter, 
    print_rules,
    enter.

% Gets the configurations for the game
configurations(S, IsAi, PAi) :- 
    get_board_size(S), 
    get_game_players(IsAi, PAi).
    