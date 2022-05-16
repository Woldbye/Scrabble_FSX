module internal Entities
  open ScrabbleUtil
  open Parser
  open MultiSet
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
  val emptyMovement: Movement;
  val updateMovement: Movement -> int -> int -> Movement
  val nextPos: Movement -> Movement
  val prevPos: Movement -> Movement
  val getPos: Movement -> coord
  val getPerpCoords: Movement -> (coord * coord)
  val mkMovement: (int * int) -> Dir -> Movement  
  val mkHook: Movement -> word -> int -> Hook

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
    turns         : uint32
    timeout       : uint32
  }