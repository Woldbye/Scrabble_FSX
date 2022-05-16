// This file is new and hides the implementation details of the parser.

module internal Parser
    open Eval
    open ScrabbleUtil
    open StateMonad
    
    type word   = (char * int) list
    type square = Map<int, squareFun>
    
    type boardFun2 = coord -> square option
        
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
    }

    val mkBoard : boardProg -> board