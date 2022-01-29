# PFL-2021-PROLOG

## PROLOG Game made for PFL UC 

> Henrique Costa Sousa

> Antonio Ribeiro

> Group: JesonMor_5


### Game description: Jeson Mor (_9 horses_)

- Played on a 9x9 checkerboard with horse pieces that move as the Chess Knight. 
- Winning is achieved by moving one of the player's horses to the middle cell of the board, proceeding to move it outside into another cell. 
- Capturing all the opponents pieces also counts as a win. 

#### Taken from 'The Encyclopedia of Chess Variants': 

 > Jesön Mor. Mongol game (literally ‘NineHorses’). Board 9x9; each player has nine knights arranged on respective first ranks. Objective is to be the first to occupy the central square (e5); usual displacement capture. Presumably a player would have to occupy the square for one turn in order to win. (Assia Popova, Jeux de Calculs Mongols, 1974) [_Chap 32.2, pag 297_]
 

### Installation and Execution 

- The SICStus interpreter must be installed to compile the Prolog files.
- The game works correctly on the Windows and Linux operating systems.
- After loading the main.pl with consult or the SICStus -l flag, the game is started by the predicate __play__.
- Included libraries:
    - random (for the players' "coin flip" and random move selection).
    - lists for list manipulation.

### Game Logic 

#### GameState 

Gamestate -> [Board, P, W, [IsAi, PAi]]
- Game state is implemented as an array form, passed down to each function in the main game logic functions.
- It includes the following elements: the board data structure, BI; the current player, P (p1 or p2); the previous player in the middle, W; and the Ai object, [IsAi, PAi] (level ai1, ai2 and its corresponding player according to the initial configuration p1 or p2).
- The Board is described below
- The P variable determines whose turns it is: the player 1, playing the whites; or the player 2, playing the blacks.
- Since one of the winning conditions is that a player who moved out of the center cell wins the game, we had to somehow remember the event from the previous board (before the move was made). This allows us to verify which player succeded in doing so (see check_player_moved_out(BI, P, W) in game.pl). If the verification matches, that player wins the game. This record is kept throught the variable W.
- The AI object is a list with two varibles: [IsAi, PAi]. The first indicates whether or not the game being played is against an AI. It will be set to `n` if there is no AI and to aiX if there is, where the X represents the difficulty (0 or 1). PAi is only used in case there is an AI player (if there isn't it will be set to the arbitrary value of `p1`). If the AI is the first player it will be `p1` otherwise it will be `p2`.

- Initial state. Each player pieces are at the top and bottom. The middle of the board, the `x` is still empty.  
```
[  
    [  
        [b,b,b,b,b,b,b,b,b],  
        [e,e,e,e,e,e,e,e,e],  
        [e,e,e,e,e,e,e,e,e],  
        [e,e,e,e,e,e,e,e,e],  
        [e,e,e,e,x,e,e,e,e],  
        [e,e,e,e,e,e,e,e,e],  
        [e,e,e,e,e,e,e,e,e],  
        [e,e,e,e,e,e,e,e,e],  
        [w,w,w,w,w,w,w,w,w]  
    ],  
p1,e,[n|p1]]  
```

- Intermediate state. One of the players has reached the middle.  
```
[  
    [  
        [b,b,e,b,b,b,b,b,b],  
        [e,e,e,e,e,e,e,e,e],    
        [e,e,e,b,e,e,e,e,e],   
        [e,e,e,e,e,e,e,e,e],    
        [e,e,e,e,w,e,e,e,e],  
        [e,e,e,e,e,e,e,e,e],       
        [e,e,e,e,e,e,e,e,e],  
        [e,e,e,e,e,e,e,e,e],  
        [w,w,e,w,w,w,w,w,w]  
    ],  
p2,x,[n|p1]]   
```
- Final state. A player has moved out of the middle. The final state is not printed as it is just relevant to know that the game has been one. Internally it is represented as follows.  
```
 [  
     [  
        [b,b,e,b,b,b,b,b,b],  
        [e,e,e,e,e,e,e,e,e],  
        [e,e,e,b,e,e,e,e,e],   
        [e,e,e,e,e,e,e,e,e],  
        [e,e,e,e,e,e,e,e,e],  
        [e,e,e,e,e,e,e,e,e],    
        [e,w,e,e,e,e,e,e,e],  
        [e,e,e,e,e,e,e,e,e],  
        [e,w,e,w,w,w,w,w,w]  
    ],  
 p2,b,[n|p1]]  
```
#### Board 

- Our game rules specify that the game should have a center cell. For that to happen the size of the board must be LxL where L is the side o fthe board. It must also have a size between 5 and 25 included.
- The board is represented as a list of lists. The value of a piece in the board is an atom of the list. 
- The pieces are represented by the atoms of the board. A `w` will represent a white piece, a `b` a black one, a `e` an empty cell. There is also a single `x` atom which represents the middle of the board.

#### Board and board creation 

- Board creation is made with the suggested __initial_state(Size, IsAi, P, GameState)__ predicate. 
- It receives a given Size (for width and length) from a user input (default is 9), and instanciates a new GameState variable.
- The initial GameState has a default board (shown above), an empty middle cell (`e`), the AI options given by the user and p1 as the first player (starts every time as the white pieces).
- The board size belongs to the inverval [5, 26[ (1 and 3 are not coherent decisions and 26 is the alphabet limit) and has to be an odd number (a middle cell has to exist).

#### Visualizing the game state

- The game state is displayed with __display_game(GameState)__. 
- The variable is deconstructed in order to extract the board. 
- Prints out a grid style board, with an lettered top row (that indicates the columns) and a numbered left side column (indicates the rows).
- An x marks the center cell and `-` and `|` provide a clear separation of the board. 
- User display is done through __get_player_move(GameState, L1, C1, L2, C2)__.

#### Menus 

- When the user executes the application, he will be shown two introductory menus, presenting the game and the rules.
- He will then be able to choose a board size, the default being 9.
- He can then choose whether or not he will be playing against an AI.
    - If that is the case, first he is to choose the level of the AI and then whether or not the AI will go first
    - If that is not the case, the players mush each input their names. After choosing the names, the players can decide who will go first or the first player will be chosen at random.
- With the configurations chosen, the game will start.

#### Validating input 

- Validation is present in all input forms.
- A failure driven loop paradigm is used to verify the numbers/characters the user has entered, and if they are valid according to the restrictions, repeating the question until a successfull answer.
- __read_line(L)__  predicate reads the input (without the need for a end dot), which is later parsed. 
- Different use cases are considered: the enter (`\n`) input, an `y` or `n` question, numbered input or a word are all taken into account (in their specific context, ascii codes, valid limits etc).

#### Move validation

- We selected the format LStart CStart - LEnd CEnd, to be the most appropriate one to parse through. Once again, occasions such as more or less spaces and invalid words/formats are taken in consideration. Negative and offlimit numbers are also discarted in this layer, that corresponds to the predicate __get_move(P, L1, C1, L2, C2)__. 
- In the game logic validation aspect is controlled by the predicate __get_player_move(GameState, L1, C1, L2, C2)__, that repeats itself until valid coordinates are inserted.
- We verify the following constraints:  
    - Through __check_permission_move(L1,C1,BI,P)__, if the selected piece is our color (b or w).
    - Through __check_bounded_move(L2,C2, Length)__, if the board bounds are not exceeded. 
    - Through __check_where_to_move(L2,C2,BI,S)__, if the final position overlaps with one of our pieces. 
    - Through __check_viable_move(L1,C1,L2,C2)__, if the move pattern corresponds to a valid L shape. This predicate is also very useful when instatiating end coordinates from given L1 and C1.  

#### Move execution 

- We assume that the player of the AI have already provided a Move.
- A Move is a list with 4 elements: [L1,C1,L2,C2]. (L1,C1) are the numerical coordinates of the piece that is moving and (L2,C2) the numerical coordinates of the place the piece is moving to. 
- The move execution will call the predicate __move(GameState, Move, NewGameState)__.
- First we will change the status of the Board by calling the function __do_move(S,L1,C1,L2,C2,BI,BO)__. BO will be returned
    - The __do_move__ function simply consists of __make_symbol(e,L1,C1,BI,BM)__, __make_symbol(S,L2,C2,BM,BO)__, that will change the symbol where the piece comes from to an `e` representing an empty space; and the symbol the pieces goes to to the respective player symbol, S.
- We then change the player that is playing with the function __change_player(P, P0)__. P0 will be returned
- We then update the value of W by checking the symbol in the middle of the resulting Board with the functoins __get_middle(BI, M)__, __is_symbol(W, M, M, BI)__. W will be returned.

#### Valid moves 

- As explained above, if we insert a pair of start coordinates ([L1, C1]), without instantiating the L2 and C2 variables, the predicates used by input validation will also output some move options (the __check_viable_move(L1,C1,L2,C2)__ comparisons are done through `is`, and all the rest validate through list maniputation/search which includes returning coordinates that fall into a specific condition). 
- We start off by executing the __valid_moves(GameState, ListOfMoves)__ predicate, that makes the necessary variable extrations from GameState and searches for the player's own pieces that are currently in the board (all [L1, C1] pairs, pruned by __is_symbol(S,L1,C1,BI)__). 
- __all_moves(BI, S, [[L1, C1] | RestOfPieces], ListOfMoves)__ receives this list and uses a recursive algorithm to finally return the list of all available moves: 
    - A findall searches for the [L1, C1, L2, C2] coordinates that match __check_ai_move(BI,S, L1, C1,L2, C2)__ (includes the predicates that are used for move validation). 
    - A recursive call continues this process for the rest of the pieces coordinates list.
    - The result is concatenated with the list in this predicate recursive stack. 

Note: the predicate mentioned that generates moves is executed in __get_game_move(GameState, L1, C1, L2, C2)__, where the game will decide to ask the user or create its own move (if AI in GameState is selected).


#### Game Over

- Game over and the game winner is discovered through the __game_over(GameState, Winner)__ predicate, that is the first pattern matching predicate of __game_loop(GameState)__ . 
- It extracts the necessary variables from the GameState. 
- It evaluates the board according to two criteria (the winning situations): 
    - __check_players_pieces(BI,P)__, the current player no longer has pieces to move. 
    - __check_player_moved_out(BI, P, W)__, the W variable in GameState shows if a W colored piece was in the middle cell in the previous game state. If the opposite player made a move that left this cell empty, the game is finished and the current player loses. 
    - When both verify it changes the winner to the Player.

#### Evaluating the game state 

- Before evaluating the board, we must ensure that the AI does not have a piece in the middle of the board. If so, it wins the game simply by moving outside and no evaluation is required. 
- The game state evaluation algorithm takes a Gamestate as input and returns the associated value __get_board_value(GameState, Value)__: . __The lower the better__. There is a series of steps involved to introduce some nuance and better decision. Some of these steps were given a bigger weight as they were deemed more important.
    - The pieces of the AI have a negative value of 100.
    - The pieces of the player have a positive value of 100.
    - The pieces of the AI adjactent to the middle have a negative value of 500.
    - The pieces of the player adjacent to the middle have a positive value of 500.
    - The number of ways every piece of the AI can be eaten by one of the player has a positive value of 500. 
    - Game ending condition has either a decisive (will override everything else) positive or negative value.
        - If the player has a piece in the middle, the board is given a decisive positive value. The game will be over and lost in the next turn
        - If the AI has a piece in the middle.
            - The player has a number of pieces adjacent to the middle bigger than the AI's. It means that there will be trading of pieces in the middle square but the player will win because he has a greater number. The board is given a decisive positive value and the game will be over and lost in a series of turns (assuming the player plays perfectly).
            - Otherwise, the board is given a decisive negative value and the game will be over and won in a series of turns (the AI will play optimally).

#### Choose Moves 

- To choose the move of the AI, the __get_ai_move__ function is called. According to the level of the AI it will behave according to the following

##### Random - AI Lvl 0

- The move chosen will be a random one from the ListOfMoves obtained by the __valid_moves__ explained previously.

##### Best move - AI Lvl 1

- First the AI verifies if it can win. If so, it will always play in that way.
- If the AI cannot win, it will choose the best win according to the state evaluation algorithm described before. To do that, it will generate the gamestate associated with each move it can take and choose the one with the smallest value, the best move.

### Conclusion 

- The PROLOG logic paradigm provides useful features that do not compare to other languages. The predicate style of writing, allows for a much intuitive and cleaner restriction checking and the "automatic" variable instatiation was extremely important when creating valid moves that fell under specific constraints. 
- However, the debugging proccess can sometimes be very tiresome and confusing. 
- On the other hand, we were unable to design a game loop that wasn't recursive due to the inability to reassign variables. This didn't pose a problem coding-wise but it raises the question: could the players exceed the recursion limit by playing back and forth?
- We also think that our AI algorithm is challenging and will take the optimal action in a variety of cases, making it a force to be reckoned with!

### References 

List manipulation: https://www.swi-prolog.org/pldoc/doc_for?object=nth0
The SWI prolog documentation: https://www.swi-prolog.org/
The SICStus prolog documentation: https://sicstus.sics.se/sicstus/docs/latest4/html/sicstus.html/
Input conversion (atom_codes): https://stackoverflow.com/questions/13670798/prolog-list-of-charcodes-to-a-string-or-characters