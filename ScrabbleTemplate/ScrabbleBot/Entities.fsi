module internal Entities
    open ScrabbleUtil
    open Parser
    open MultiSet
    open Eval
    
    type move = list<coord * (uint32 * (char * int))>

    type stateDto = {
        board         : Parser.board
        dict          : Dictionary.Dict
        playerNumber  : uint32
        hand          : MultiSet.MultiSet<uint32>
        tiles         : Map<uint32, tile>
    }


    type Dir =
        | Up    // not supported
        | Down  
        | Left  // not supported
        | Right

    type Movement = {
        pos: coord
        dir: Dir
    }

    val updateMovement: Movement -> int -> int -> Movement
    val nextPos: Movement -> Movement
    val getPos: Movement -> coord