% Referenced from: https://www.swi-prolog.org/pldoc/doc_for?object=nth0/4

% Replaces the element of a list returning a new list. Used to update the board after a move
replace(List, Index, OldElem, NewElem, NewList) :-
   % predicate works forward: Index,List -> OldElem, Transfer
   nth0(Index,List,OldElem,Transfer),
   % predicate works backwards: Index,NewElem,Transfer -> NewList
   nth0(Index,NewList,NewElem,Transfer).