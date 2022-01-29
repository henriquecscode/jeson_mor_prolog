:-ensure_loaded('./game.pl').

%GameState(board, player, winnable, [IsAI with diffculty, FirstAi])

%AI predicates

% Gets the Move of the AI
get_ai_move(GameState, L1, C1, L2, C2):-
   convert_game_state_to_var(GameState, _, _, _, [IsAi | _]),
   valid_moves(GameState, ListOfMoves),  
   (
       (IsAi = ai1, get_ai_random_move(ListOfMoves, [L1, C1, L2, C2]))
       ;
       (IsAi = ai2, get_best_move(GameState, ListOfMoves, [L1, C1, L2, C2]))
   ).

% Gets the move of the AI if the level 1 is selected
% If has a winning move, will give that one
% Otherwise, will choose the best move according to the game state evaluation algorithm
get_best_move(GameState, ListOfMoves, Move):-
    (
        has_winning_move(GameState),
        get_winning_move(GameState, Move),
        member(Move, ListOfMoves)
    )
    ;
    (
        get_ramification_move(GameState, ListOfMoves, Move)
    )
    .


% Gets a random move from a list of moves
get_ai_random_move(ListOfMoves, Move) :-
    random_select(Move, ListOfMoves, _Rest).


% Gets the valid moves the AI can make from a certain GameState
% First gets all the pieces
% Then gets all the moves the pieces can make
valid_moves(GameState, ListOfMoves):- 
    convert_game_state_to_var(GameState, BI, _, _, [_ | PAi]),
    player_symbol(PAi, S), 
    findall([L1, C1], is_symbol(S,L1,C1,BI), Pieces),  
    all_moves(BI, S, Pieces, ListOfMoves).


% All the moves that are possible to make with the pieces available
all_moves(_, _, [], []).
all_moves(BI, S, [[L1, C1] | RestOfPieces], ListOfMoves):- 
    findall([L1, C1, L2, C2], check_ai_move(BI,S, L1, C1,L2, C2), NewMove),
    all_moves(BI, S, RestOfPieces, NewListT), 
    append(NewMove, NewListT, ListOfMoves). 

% Checks if a move that the AI is making is possible
check_ai_move(BI,S, L1, C1, L2, C2):-
    length(BI, Length),
    check_viable_move(L1,C1,L2,C2), 
    check_bounded_move(L2,C2, Length),    
    check_where_to_move(L2,C2,BI,S).

% Checks if the AI can win the game (must have a piece in the middle square)
has_winning_move(GameState):-
    convert_game_state_to_var(GameState, _, _, W, [_ | PAi]),
    player_symbol(PAi, S),
    W = S.

% If has a winning move, will fetch it
get_winning_move(GameState, Move):-
    convert_game_state_to_var(GameState, Board, _, _, [_ | PAi]),
    player_symbol(PAi,S),
    get_middle(Board, M),
    findall([L2,C2], check_ai_move(Board, S, M, M, L2, C2),PossibleWins),
    nth0(0, PossibleWins, [L2,C2]),
    convert_move_to_var(Move, M, M, L2, C2).

% Gets the best move that the AI can make at a given moment
% For each move that the AI can make, will make it generating a new game state.
% Will analyse the new game state, giving a value to each one of those, and choosing the best
get_ramification_move(GameState, ListOfMoves, Move):-
    setof(
        Value-Mv,
        NewGameState^(
            member(Mv, ListOfMoves),
            move(GameState, Mv, NewGameState),
            get_board_value(NewGameState, Value)
            ),
        [_V-Move|_]
    ).

% Gets the value of the board where lower is better
% See the README.md for a better understanding of the rationale behind the algorithm
get_board_value(GameState, Value):-
    convert_game_state_to_var(GameState, Board, P, _, [_ | PAi]),
    get_middle(Board, M),
    player_symbol(PAi,S1),
    findall([L1, C1], is_symbol(S1,L1,C1,Board), SelfPieces),  
    length(SelfPieces, SelfPiecesLength),
    SelfPiecesValue is SelfPiecesLength * -100,

    player_symbol(P, S2),
    findall([L1, C1], is_symbol(S1,L1,C1,Board), OtherPieces),  
    length(OtherPieces, OtherPiecesLength),
    OtherPiecesValue is OtherPiecesLength * 100,

    (
       ( is_symbol(S2, M, M, Board), OtherMiddleValue is 100000);
       ( OtherMiddleValue is 0)
    ),

    (
        ( 
            is_symbol(S1, M, M, Board),
            (
                (
                    check_dispute_eatable(Board, S1, S2), SelfMiddleValue is -100000
                )
                ;
                (
                    SelfMiddleValue is 100000
                )
            )
        );
        (
            SelfMiddleValue is 0
        )
        
    ),

    findall([L1, C1], (check_viable_move(M,M,L1,C1), is_symbol(S1,L1,C1,Board)), SelfAdjacentMiddle),
    
    length(SelfAdjacentMiddle, SelfAdjacentMiddleLength),
    SelfAdjacentMiddleValue is SelfAdjacentMiddleLength * -500,
    findall([L1, C1], (check_viable_move(M,M,L1,C1), is_symbol(S2,L1,C1,Board)), OtherAdjacentMiddle),
    length(OtherAdjacentMiddle, OtherAdjacentMiddleLength),
    OtherAdjacentMiddleValue is OtherAdjacentMiddleLength * 500,
    check_eatable(Board, S1, S2, SelfPieces, EatedFrom),
    length(EatedFrom, EatedFromLength),
    SelfPiecesEatenFrom is EatedFromLength * 500,

    Value is SelfPiecesValue + OtherPiecesValue + OtherMiddleValue + SelfMiddleValue + SelfAdjacentMiddleValue + OtherAdjacentMiddleValue + SelfPiecesEatenFrom
    .


% Checks if the AI can win a dispute over the middle of the board.
check_dispute_eatable(Board, S1, S2):-
    get_middle(Board, M),
    findall([L2,C2], (check_viable_move(M, M, L2,C2), is_symbol(S2, L2, C2, Board)), CounterMoves),
    findall([L2,C2], (check_viable_move(M, M, L2,C2), is_symbol(S1, L2, C2, Board)), CoverMoves),
    length(CounterMoves, CounterMovesLength),
    length(CoverMoves, CoverMovesLength),
    CoverMovesLength >= CounterMovesLength.


% Gets all of the ways that the pieces of the AI can be eaten by pieces of the player
check_eatable(_, _, _, [], []).
check_eatable(Board, S1, S2, [[L1,C1] | SelfPieces], EatedFrom):-
        findall([L2,C2],( check_ai_move(Board, S1, L1, C1, L2, C2), is_symbol(S2, L2, C2, Board)), ThisEatedFrom),
        check_eatable(Board, S1, S2, SelfPieces, NewEatable),
        append(ThisEatedFrom, NewEatable, EatedFrom).


