module internal Bufio
    open ScrabbleUtil
    open Eval
    open Parser

    type stateDto = {
        board         : Parser.board
        dict          : Dictionary.Dict
        playerNumber  : uint32
        hand          : MultiSet.MultiSet<uint32>
        // TO:DO add player number
    }
    
    // 0 1 (INDEX_CHAR, 'A', 3)
    //type coord = int * int
    type move = list<coord * (uint32 * (char * int))>

    // Send move string ?
    val nextMove : stateDto -> move

    val isValid : move -> bool    

    val calculatePoints : square list -> tiles:word -> int