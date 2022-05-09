module internal Bufio
  open ScrabbleUtil
  open Eval
  open Parser
  open StateMonad

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
  let evalScore (mv:move) (st: stateDto) : int =
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
        if evalScore m1 s > evalScore m2 s then m1 else m2
        
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
  // Map.toList sq square.
  /// <description> 
  /// Calculate the number of points given for placing `tiles` on `square`.
  /// Both parameters are assumed to be a list of equal length, 
  /// such that count of squares in `squares` equal count of chars in `tiles`.
  /// <description/>
  /// <return> Returns number of points given for placing `tiles` above `square` <returns/>
  let calculatePoints (squares: square list) (tiles:word) : int = 
                                            // index    
    let sqMapper (i: int) (sq: square) : ((int * (int -> int)) list) =  
      sq
      |> Map.toList
      |> List.map (fun (j, sq_fun) ->
          (
            j,
            fun (acc: int) ->
              match (sq_fun tiles i acc) with
              | Success res -> res 
              | Failure f -> 0
          )
      )

    List.mapi sqMapper squares 
    |> List.fold ( fun acc ele -> acc @ ele ) []
    |> List.sortBy (fun (j, _) -> j)
    |> List.map (fun (_, ele) -> ele)
    |> List.fold (
      fun (acc: (int -> int)) (fn: (int -> int)) -> acc >> fn
    ) (fun i -> i)
    |> fun n -> 0 |> n
  