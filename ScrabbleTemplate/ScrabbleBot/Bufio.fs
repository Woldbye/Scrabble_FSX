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
  
  type move = list<coord * (uint32 * (char * int))>

  let nextMove (state: stateDto) : move = 
    // let rec maximize s : move = 
      // state
          // rec maximize stateDto : move  
          // ---> 
          // |
          // DOWN
          // WORDS ON BOARD 
          // SME WORD ON BOARD: ABE
          // HAND: R N E
          // => 
          // "A" : "B" : "E" -> "R" -> "N" -> ""
          //    HE 
          // ABE
          //  
    let mve = [(0, 0), (0u, ('A', 1))]
    mve

  /// Returns true if `mv` would be valid to play on the current state  
  let isValid (mv:move) : bool = failwith "Not implemented IsValid"
  
  
  // type squareFun = word -> int -> int -> Result<int, Error>
  // type square = Map<int, squareFun>

  // Calculate points for a given word
  //! TO:DO implement, see Assignment 2.15, 3.8 and ScrabbleProject description
  let calculatePoints (squares: square list) (tiles:word) : int = 
    0