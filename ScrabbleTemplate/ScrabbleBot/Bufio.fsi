module internal Bufio
    open ScrabbleUtil
    open Eval
    open Parser
    open Entities

    // Send move string ?
    val nextMove : stateDto -> move
  
    val calculatePoints : square list -> tiles:word -> int