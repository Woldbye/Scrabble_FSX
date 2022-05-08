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

  let emptyMove: move = [(0xDEADBEEF, 0xDEADBEEF), (0u, ('_', 0))] 

  /// Returns number of points given for playing `mv` on `state`
  //! TO:DO implement, use calculatePoints for implementation
  let moveToScore (mv:move) (st: stateDto) : int =
    match mv with 
    | emptyMove -> 0 
    | _ -> 0 // calculate move

  let nextMove (state: stateDto) : move = 
    let mve = emptyMove

    let rec max_word (s:stateDto) (w:word) (m: move) : move = failwith "Not implemented, aux function for max_move"
      // search for `w` in dictionary by using s.dict.step
    
    /// Run max_move recursively by applying backtracking
    //! word list input might be unneccessary
    let rec max_move (s:stateDto) (words:word list) : move =
      // returns maximum scoring move  
      let cmp m1 m2 : move = 
        if moveToScore m1 s > moveToScore m2 s then m1 else m2
        
      match words with
      | w::ws -> 
        let cur = max_word s w emptyMove
        let next = max_move s ws 
        cmp cur next
      | [] -> emptyMove // If no more words to check in this branch, just return empty move

    //! here we should retrieve all possible words on the board that can be extended
    let hooks: word list = [['_', 1]] 
    max_move state hooks

  /// Returns true if `mv` would be valid to play on the current state  
  //! TO:DO Implement
  let isValid (mv:move) : bool = failwith "Not implemented IsValid"
  
  // type squareFun = word -> int -> int -> Result<int, Error>
  // type square = Map<int, squareFun>

  /// Returns number of points given for placing `tiles` above `square`
  //! TO:DO implement, see Assignment 2.15, 3.8 and ScrabbleProject description
  let calculatePoints (squares: square list) (tiles:word) : int = 
    0
