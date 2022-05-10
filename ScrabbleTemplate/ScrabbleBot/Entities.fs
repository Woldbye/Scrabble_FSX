module internal Entities
    open ScrabbleUtil
    open Parser
    open MultiSet

    type move = list<coord * (uint32 * (char * int))>

    type stateDto = {
        board         : Parser.board
        dict          : Dictionary.Dict
        playerNumber  : uint32
        hand          : MultiSet.MultiSet<uint32>
        tiles         : Map<uint32, tile>    
    }

