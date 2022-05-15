module internal Entities
  open ScrabbleUtil
  open Eval 

  type Dir =
    | Up    // not supported
    | Down  
    | Left  // not supported
    | Right

  type Movement = {
    pos: coord
    dir: Dir
  }
  
  type Hook = {
    mov: Movement
    word: word
    count: int
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

  let prevPos (mov: Movement) : Movement =
    match mov.dir with
    | Up    -> updateMovement mov  0  1
    | Down  -> updateMovement mov  0 -1
    | Left  -> updateMovement mov  1  0
    | Right -> updateMovement mov -1  0

  let getPerpCoords (mov: Movement) : (coord * coord) =
    let (px, py) = mov.pos
    match mov.dir with
    | Up | Down -> ((px - 1, py), (px + 1, py))
    | _         -> ((px, py - 1), (px, py + 1))

  let getPos (mov: Movement) : coord = mov.pos

  let mkMovement p d = {
    pos = p
    dir = d
  }
  
  let mkHook mv w c = {
    mov = mv
    word = w
    count = c
  }


  type move = list<coord * (uint32 * (char * int))>

  type stateDto = {
    board         : Parser.board
    dict          : Dictionary.Dict
    playerNumber  : uint32
    playerTurn    : uint32
    hand          : MultiSet.MultiSet<uint32>
    tiles         : Map<uint32, tile>
    hooks         : Hook list 
    bricks        : Map<coord, tile>   
  }
