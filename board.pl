:- use_module(library(lists)). 
:- ensure_loaded('./state_conversions.pl'). 

% Fills a row with a given symbol
fill_row(Symbol, 1, [Symbol]).
fill_row(Symbol, Length, [Symbol | O1]) :- Length > 1, L1 is Length - 1, fill_row(Symbol, L1, O1).

% Creates an empty row
create_empty_row(Size, R) :- fill_row(e, Size, R).

% Creates a black row
create_black(Size, R) :- fill_row(b, Size, R).

% Creates a white row
create_white(Size, R) :- fill_row(w, Size, R).

% Fills the board with the respective symbols
fill_board(Size, Board) :-  create_empty_row(Size, E), S1 is Size - 2, length(Board, S1),  maplist(=(E), Board).

% Creates the board from the size
create_board(Size, B) :- 
    Size < 26,
    create_black(Size, Black), create_white(Size, White), 
    fill_board(Size, M), append([Black], M, B1), append(B1, [White], BI),
    get_middle(BI, Middle),
    make_symbol(x,Middle,Middle,BI,B).

% Assembles the initial state from the configurations (Size, IsAI and P)
initial_state(Size, IsAi, P, GameState) :-  
    \+(0 =:= mod(Size,2)),  
    create_board(Size, Board),
    convert_var_to_game_state(Board, p1, e, [IsAi|P], GameState).