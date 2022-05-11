module internal Entities
    open ScrabbleUtil
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
    
    let updateMovement mov dx dy : Movement =
        let (x, y) : coord = mov.pos
        {
            pos = (x + dx, y + dy)
            dir = mov.dir
        }
     
    let nextPos (mov: Movement) : Movement =
        match mov.dir with
        | Up    -> updateMovement mov  0 -1
        | Down  -> updateMovement mov  0  1
        | Left  -> updateMovement mov -1  0
        | Right -> updateMovement mov  1  0
    
    let getPos (mov: Movement) : coord = mov.pos
