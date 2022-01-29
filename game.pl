:- use_module(library(lists)). 
% :- ensure_loaded('./lists.pl'). 
% :- ensure_loaded('./view.pl').
:-ensure_loaded('./ai.pl').

get_middle(Board, M) :- length(Board, N), M is div(N,2).

% Checks if a set of coordinates in the board is a given symbol
is_symbol(S,L1,C1,BI):-
    nth0(L1,BI,L),
    nth0(C1,L,S).

% Checks if a set of coordinates in the board is a black piece
is_black(L1,C1,BI):-
    is_symbol(b,L1,C1,BI).

% Checks if a set of coordinates in the board is a white piece
is_white(L1,C1,BI):-
    is_symbol(w,L1,C1,BI).

% Checks if a set of coordinates in the board is an empty square
is_empty(L1,C1,BI):-
    is_symbol(e,L1,C1,BI); is_symbol(x, L1, C1, BI).


% Cheks which player is playing
is_black_playing(P):- P = p2.
is_white_playing(P):- P = p1.

% Gives the player symbol, white piece for the first player and black piece for the second player
player_symbol(p2,b).
player_symbol(p1,w).

% Changes the player after a turn has been made
change_player(X,Y):- X = p1,Y = p2.
change_player(X,Y):- X = p2,Y = p1.

% Moves a piece in the board by replacing the atoms in the Board
make_symbol(S,L1,C1,BI,BO):-
    nth0(L1,BI,L),
    replace(L,C1,_,S,L2),
    replace(BI,L1,_,L2,BO).

% Moves a black piece to a square of the board
make_black(L1,C1,BI,BO):-
    make_symbol(b,L1,C1,BI,BO).

% Moves a white piece to a square of the board
make_white(L1,C1,BI,BO):-
    make_symbol(w,L1,C1,BI,BO).

% Cleans a square of the board
make_empty(L1,C1,BI,BO):-
    make_symbol(e,L1,C1,BI,BO).

% Gets the coordinates of the the next Move.
% If it is the player playing it will ask for input, otherwise the ai will calculate the next move
get_game_move(GameState, L1, C1, L2, C2):-
    convert_game_state_to_var(GameState, _, P, _, [IsAi | PAi]),
    ( 
        ( IsAi \= n,
                    (
                        (P = PAi, get_ai_move(GameState, L1, C1, L2, C2)) 
                        ; 
                        (P \= PAi, get_player_move(GameState, L1, C1, L2, C2))
                    )
        )
        ; 
        (IsAi = n, get_player_move(GameState, L1, C1, L2, C2))
    ).

% Will ask for input from the player. Will verify that the move that he is trying to make is indeed allowed in the current state of the game
get_player_move(GameState, L1, C1, L2, C2):-
    convert_game_state_to_var(GameState, BI, P, _, _),
    length(BI, Length),
    repeat,
        get_move(P, L1, C1, L2, C2),
        check_permission_move(L1,C1,BI,P),
        check_bounded_move(L2,C2, Length),
        player_symbol(P,S),
        check_where_to_move(L2,C2,BI,S),
        check_viable_move(L1,C1,L2,C2),
        !
    .

%  Will execute the move that was given
move(GameState, Move, NewGameState):-
    convert_game_state_to_var(GameState,BI,P,_,Ai),
    convert_move_to_var(Move,L1,C1,L2,C2),
    play_move(L1,C1,L2,C2,BI,P,BO,PO,WO),
    convert_var_to_game_state(BO,PO,WO,Ai,NewGameState).

% Moves the piece in the board, changes the player that is playing and updates the variable that keeps track of the middle of the board
play_move(L1,C1,L2,C2,BI,P,BO,PO,W):-
    player_symbol(P,S),
    do_move(S,L1,C1,L2,C2,BI,BO),
    change_player(P,PO),
    get_middle(BI, M),
    is_symbol(W, M, M, BI).

% Checks whether or not the piece in the indicated coordinate of the board belongs to the player
check_permission_move(L1,C1,BI,P):-
    (is_black_playing(P),is_black(L1,C1,BI));
    (is_white_playing(P),is_white(L1,C1,BI)).

% Checks whether or not the move is to a square withint the b
check_bounded_move(L2,C2, Length):-
    L2 >= 0, L2 < Length, C2 >= 0, C2 < Length.

%Checks whether or is trying to overlap pieces belonging to the player
check_where_to_move(L2,C2,BI,S):-
    \+is_symbol(S,L2,C2,BI).

% Checks whether the move abides by how a horse moves
check_viable_move(L1,C1,L2,C2) :- 
    (L2 is L1 - 1, C2 is C1 - 2) ;
    (L2 is L1 + 1, C2 is C1 - 2) ; 
    (L2 is L1 - 1, C2 is C1 + 2) ;
    (L2 is L1 + 1, C2 is C1 + 2) ; 
    (L2 is L1 - 2, C2 is C1 - 1) ;
    (L2 is L1 + 2, C2 is C1 - 1) ; 
    (L2 is L1 - 2, C2 is C1 + 1) ;
    (L2 is L1 + 2, C2 is C1 + 1).

% Moves the piece by replacing the atom in the Board
do_move(S,L1,C1,L2,C2,BI,BO):-
    make_symbol(e,L1,C1,BI,BM),
    make_symbol(S,L2,C2,BM,BO).
    
% Checks if the current state is a game over by either verifying if the current players has run out of pieces or the other player has moved one of his pieces out of the middle square
game_over(GameState, Winner):-
    convert_game_state_to_var(GameState, BI, P, W, _),
    (
    check_players_pieces(BI,P);
    check_player_moved_out(BI, P, W)
    ),
    change_player(P, Winner).

% Checks if the player moved one of his pieces out of the middle square
check_player_moved_out(BI, P, W):-
    change_player(P,WP),
    player_symbol(WP,S),
    W = S,
    get_middle(BI,M),
    is_empty(M,M,BI).

% Checks if the player still has pieces
check_players_pieces(B2,P):-
    player_symbol(P,S),
    no_pieces(S,B2).

% Recursively goes through the board and sees if the player has no more pieces
no_pieces(_,[]). 
no_pieces(S,[C | R]) :- 
    no_pieces_row(S,C),
    no_pieces(S,R).
% Checks if the player no longer has pieces in that row
no_pieces_row(S,R) :- 
    \+nth0(_,R,S).
