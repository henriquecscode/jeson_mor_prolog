:- include('./board.pl'). 
:- include('./game.pl').
:- include('./lists.pl').
:- include('./tests.pl').
:- ensure_loaded('./view.pl').
:- include('./menus.pl').
:-ensure_loaded('./ai.pl').

% Application start
play:-
    start_menu,
    configurations(S, IsAi, PAi),
    initial_state(S, IsAi, PAi, GameState),
    game_loop(GameState).

% Part of the game loop that verifies if the game has come to an end. If so, then the program will terminate after printing the winner.
game_loop(GameState):-
    game_over(GameState, Winner), !,
    print_winner(Winner).

% Part of the game loop that gets a move in case the game did not end.
% Will print the gamestate, get the move of the player, execute it and then proceed to the next turn
game_loop(GameState):-
    display_game(GameState),
    get_game_move(GameState, L1, C1, L2, C2),
    convert_move_to_var(Move, L1,C1,L2,C2),
    move(GameState, Move, NewGameState),
    game_loop(NewGameState).