module internal MoveGen
  open ScrabbleUtil
  open Eval
  open Parser
  open StateMonad
  open Entities

    
  // Ordered List of ids
  type moveDto = uint32 list

  // type move = list<coord * (uint32 * (char * int))>
  // assumes w not empty
  let prefix (w:word) : char = w.Head |> fun (c,v) -> c
  let emptyMove: move = []  
  let emptyMoveDto: moveDto = []

  /// Returns number of points given for playing `mv` on `state`
  //! TO:DO implement, use calculatePoints for implementation
  let evalMove (st: stateDto) (mv:move) : int =
    match mv with 
    | emptyMove -> 0 
    | _ -> 0 // calculate move

  /// <returns>
  /// None if word is not contained in dict or word is empty
  /// Some d where d is outgoing node from following `w`
  /// <returns/>
  let stepWord (word: word) (d:Dictionary.Dict) : option<bool * Dictionary.Dict> = 
    let rec aux (w: word) (res:option<(bool * Dictionary.Dict)>) : option<bool * Dictionary.Dict> =
      match (res, w) with 
      | (Some (_, next), _) -> 
        ((w |> prefix), next) 
        ||> Dictionary.step 
        |> aux w 
      | _ -> res

    match word with 
    | [] -> None
    | _ -> aux word.Tail (Dictionary.step (word |> prefix) d)
    
  // start (0,0) => w
  // end (4, 0) => 
  // w h e n
  // a     i
  // s     g
  //       g
  //       e 
  // w h e r e 

  // MAX_END_POINT
  // START POINT -> END POINT
  let getMinTile (state: stateDto) (cid: uint32): (char * int) = 
    state.tiles.[cid] |> fun t -> t.MinimumElement
  
  /// <returns> 
  /// Best scoring playable move that extends `word`, 
  /// if no extension available `emptyMove` 
  /// <returns /> 
  let bestExtension (state: stateDto) (word:word) (maxMove: moveDto -> moveDto -> moveDto) : moveDto =     
    // TECHNIQUE: BACKTRACKING:
    // GOAL: RUN ALL POSSIBLE EXTENSIONS OF INPUT WORD
    let rec loopHand (hand: MultiSet.MultiSet<uint32>) (dict:Dictionary.Dict) (cids:uint32 list) (acc:moveDto) : moveDto =
      match cids with 
      | [] -> emptyMoveDto // end recursion 
      | cid :: tail -> // cid = char id
        
        // Skip branch
        // TODO Add current id to stop inf loop
        let m1 : moveDto = loopHand hand dict (tail @ [cid]) acc //! TO:DO add stop for inifinity loop

        // Remove played tile from hand
        let hand' = hand |> MultiSet.removeSingle cid;

        // extract char from tile
        let key : char = getMinTile state cid |> fun (c', _) -> c'
        
        // Update move
        let mv' : moveDto = acc @ [cid] 

        // Try play char
        let m2 : moveDto = 
          match (dict |> Dictionary.step key) with 
          // No finished word, check sub-branches
          | Some (false, d') -> loopHand hand' d' tail mv'
          | Some (true, d')  -> loopHand hand' d' tail mv' |> maxMove mv'
          // No possible play in sub-branch 
          | None -> emptyMoveDto
        maxMove m1 m2
        
    // Ids -> state.tiles.[id]
    let handIds = MultiSet.toList state.hand |> List.map (fun (id, _) -> id) 
    
    // all available chars on hand
    match (stepWord word state.dict) with 
    | None -> emptyMoveDto
    | Some (_, d) -> loopHand state.hand d handIds emptyMoveDto

  // Convert the input movedto to a move
  let toMove (state: stateDto) (mm: Movement) (dto: moveDto) : move =
    let rec loopDto (mov: Movement) (mdto: moveDto) (acc: move) : move = 
      match mdto with 
      | m :: ms ->
        let nextM = mov |> nextPos
        
        getMinTile state m
        |> fun w -> (m, w)
        |> fun h -> acc @ [( nextM |> getPos, h )]
        |> loopDto nextM ms
      
      | [] -> acc

    loopDto mm dto emptyMove
    

  let max (state: stateDto) (mov: Movement) (m1: moveDto) (m2:moveDto) : moveDto =
    let ev = evalMove state
    if evalMove st m1 > evalMove st m2 then m1 else m2

  let bestMove (state: stateDto) : move = 
    // Run max_move recursively by applying backtracking
    let rec aux (s:stateDto) (words:word list) : move =
      match words with
      | w::ws -> 
        let cur = bestExtension s w 
        let next = aux s ws 
        max s cur next
      | [] -> emptyMove // If no more words to check in this branch, just return empty move

    //! here we should retrieve all possible words on the board that can be extended
    let hooks: word list = [['_', 1]] // Words currently on the board
    aux state hooks

  /// Returns true if `mv` would be valid to play on the current state  
  //! TO:DO Implement
  let isValid (mv:move) : bool = failwith "Not implemented IsValid"
  