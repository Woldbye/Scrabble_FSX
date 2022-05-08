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

  /// <description> 
  /// Calculate the number of points given for placing `tiles` on `square`.
  /// Both parameters are assumed to be a list of equal length, 
  /// such that count of squares in `squares` equal count of chars in `tiles`.
  /// <description/>
  /// <return> Returns number of points given for placing `tiles` above `square` <returns/>
  //! TO:DO implement, see Assignment 2.15, 3.8 and ScrabbleProject description
  let calculatePoints (squares: square list) (tiles:word) : int = 
    // INPUT example
    // <TILES> QIN ([('Q', 10); ('I', 1); ('N', 1)])
    // <SQUARES> 
    // HEAD 1. map[(0, TLS)]
    // 2. map[(0, SLS)] 
    // TAIL 3. map[(0, SLS); (1, DWS)];
    let tmp = (squares, tiles) ||> List.mapi2 (fun i sqList tileList -> i, ) 
    // sq = Map.toList sq
               
    let rec evalWord sqList tileList acc =
      match squares with 
      | sq::sqs -> 
        // loop through entire map in @sq
        let tile_score = tileList.Head |> fun (tc, ts) -> ts 
        let sq_score = 
          Map.toList sq |> 
          List.map (fun (_, sqFun) ->   sqFun) 
        tileList.Head |> evalWord sqs tileList.Tail 
      | []      ->  
//       0

// 1. List.mapi : (int -> 'T -> 'U ) -> 'T list -> U list that given a function mapper and a list
// [x0; x1; ...; xn] returns the list [mapper 0 x0; mapper 1 x1; ...; mapper n xn] - this allows
// mapper to operate on an index as well as the elements at that index of the list. Otherwise it works like
// regular maps.
// 2. List.sortBy : ('T -> 'U) -> 'T list -> 'T list that given a projection function proj and a
// list lst returns a sorted version of lst where the sorting is based on first applying proj to each
// element of the list. Note that the projection is used only for sorting and the elements of the list does
// not change - only their order

// Create a function calculatePoints : square list -> word -> int that given a list of squares squares
// and a list of tiles w calculates the number of points that w gives you when placed over squares . The lists
// squares and w are of equal length and you do not have to take care of the cases when they are not.
// This assignment is at first glance daunting, but with the help of higher-order functions it is actually very
// short and the steps detailed below can be combined either by using the piping operator (|>) or function
// composition (>>) .
// Note that the type square list is identical to the nested list ((int * squareFun) list) list since the
// type square is a list itself.

// STEPS:
// 1. Use mapi on the square list and map on the nested square , to create a list of type 
//    ((int * (int -> int)) list) list where the priorities in squares have been left intact and the functions have
//    been partially applied with word and the correct index.
// 2. Flatten: Fold the list of lists into a single list:  (turn ((int * (int -> int) list) list 
// 3. Sort: Use sortBy to sort this list based on priority (lowest to highest)
// 4. Use map to discard the priority leaving you with a list of type (int -> int) list
// 5. Use fold and function composition ( >> ) to compose all functions in the list resulting in one single
// function of type int -> int
// Instantiate the function with 0 for the initial accumulator to calculate your points.






// > calculatePoints [DLS; SLS; TLS; SLS; DWS] hello;;
// - val it : int = 28
// > calculatePoints [DLS; DWS; TLS; TWS; DWS] hello;;
// - vil it : int = 168