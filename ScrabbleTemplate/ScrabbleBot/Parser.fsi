// This file is new and hides the implementation details of the parser.

module internal Parser
    
    open Eval
    open ScrabbleUtil
    open FParsecLight
    
    type word   = (char * int) list
    type square = Map<int, word -> int -> int -> int>
    type boardFun = coord -> square option
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun
    }

    val parseBoardProg : boardProg -> board