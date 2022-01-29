% A test file we used to incrementally test some of our functions

test_check_permission_move(L1,C1,P):-
    create_board(9,BI),
    check_permission_move(L1,C1,BI,P).

test_check_where_to_move(L2,C2,S):-
    create_board(9,BI),
    check_where_to_move(L2,C2,BI, S).

test_play_move(L1,C1,L2,C2,P,BO,PO):-
    create_board(9,BI),
    play_move(L1,C1,L2,C2,BI,P,BO,PO,_).

test_check_middle_end_game(Winner):-
    create_board(9,BI),
    do_move(b,0,0,4,4,BI,B1),
    play_move(4,4,2,3,B1,p2,BO,P1,W),
    convert_var_to_game_state(BO, P1, W, _, GameState),
    game_over(GameState, Winner).
%Yields X = p2?

test_check_middle_end_game2(Winner):-
    create_board(11,BI),
    do_move(b,0,0,5,5,BI,B1),
    play_move(5,5,3,4,B1,p2,BO,P1,W),
    convert_var_to_game_state(BO, P1, W, _,GameState),
    game_over(GameState, Winner).
%Yields X = p2?

check_get_symbol(X):-
    create_board(9,BI),
    is_symbol(X, 4, 4, BI).

check_get_symbolB(X):-
    create_board(9,BI),
    is_symbol(X, 0, 4, BI).

check_get_symbolW(X):-
    create_board(9,BI),
    is_symbol(X, 8, 4, BI).

check_log(X,Y):-
    Y is floor(log(10, X)) + 1.

check_print_n(X):-
    print_n('a', X).

check_display_columns(X):-
    display_columns(X).


check_convert_move_to_var(C1, L1, C2, L2):-
    convert_move_to_var(Move, 1, 2, 3, 4),
    convert_move_to_var(Move, C1, L1, C2, L2).

check_configurations(S):-
    configurations(S).


test_get_ai_move(L1, C1, L2, C2):- 
    initial_state(9, ai1, p1, GameState),
    get_ai_move(GameState, L1, C1, L2, C2).
    