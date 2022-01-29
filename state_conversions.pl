:- use_module(library(lists)). 

% Converts a GameState to a Board (BI), the current player (P), the previous piece in the middle of the board (W), and the AI configuration (AI)
convert_game_state_to_var(GameState, BI, P, W, AI):-
    nth0(0, GameState, BI),
    nth0(1, GameState, P),
    nth0(2, GameState, W),
    nth0(3, GameState, AI).

% Does the bidirectional conversion between a move and its coordinates
convert_move_to_var(Move, L1,C1,L2,C2):-
    nth0(0, Move, L1),
    nth0(1, Move, C1),
    nth0(2, Move, L2),
    nth0(3, Move, C2).

% Converts the GameState variables into the GameState
convert_var_to_game_state(BI, P, W, AI, GameState):-
    append([[BI], [P], [W], [AI]], GameState).
