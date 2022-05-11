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


  /// <description> 
  /// Calculate the number of points given for placing `tiles` on `square`.
  /// Both parameters are assumed to be a list of equal length, 
  /// such that count of squares in `squares` equal count of chars in `tiles`.
  /// <description/>
  /// <return> Returns number of points given for placing `tiles` above `square` <returns/>
  let calculatePoints (squares: square list) (tiles:word) : int =    
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

  // 
  // ['a',0]


  let moveToWord (mv: move) : word = List.map (fun (_, (_, w)) -> w) mv
  
  let moveToHook (mv: move) : Hook =
    let w = moveToWord mv
    let (c0, _) :: (c1, _) :: _ = mv
    
    let dir =
      match (c0, c1) with
      | ((x0, _), (x1, _)) when x0 = x1 -> Dir.Down
      | _                               -> Dir.Right
    
    



  /// Returns number of points given for playing `mv` on `state`
  let evalMove (st: stateDto) (mv:move) : int =
    let folder (acc: (square list) option) (c, _) : ((square list) option) =
      match (st.board.squares c) with
        | Success sqOpt when (sqOpt.IsSome && acc.IsSome)
            -> Some (acc.Value @ [sqOpt.Value])
        | _ -> None
      
    match mv with 
    | m when m = emptyMove -> 0 
    | _ -> 
      mv
      |> List.fold folder (Some [])
      |> function
        | Some squares -> mv |> moveToWord |> (squares |> calculatePoints)
        | None -> 0
      
    // st.board.squares (st.board.center) |> 
    // calculatePoints mv  

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
    let rec loopHand (hand: MultiSet.MultiSet<uint32>) (depth: int) (dict:Dictionary.Dict) (cids:(int * uint32) list) (acc:moveDto) : moveDto =
      match cids with 
      | [] -> emptyMoveDto // end recursion
      | (d, _) :: _ when d > depth -> emptyMoveDto
      | (_, c) :: tail when MultiSet.contains c hand = false -> loopHand hand depth dict tail acc
      | (d, cid) :: tail -> // cid = char id
        // Skip branch
        let m1 : moveDto = loopHand hand depth dict (tail @ [(d + 1, cid)]) acc

        // Remove played tile from hand
        let hand' = hand |> MultiSet.removeSingle cid;
        
        // extract char from tile
        let key : char = getMinTile state cid |> fun (c', _) -> c'
        
        // Update move
        let mv' : moveDto = acc @ [cid] 

        // Try play char
        let recHandLoop dd = loopHand hand' (depth + 1) dd cids mv'

        let m2 : moveDto = 
          match (dict |> Dictionary.step key) with 
          // No finished word, check sub-branches
          | Some (false, d') -> recHandLoop d'
          | Some (true, d')  -> recHandLoop d' |> maxMove mv'
          // No possible play in sub-branch 
          | None -> emptyMoveDto
        maxMove m1 m2
        
    // Ids -> state.tiles.[id]
    let handIds = MultiSet.toList state.hand |> List.map (fun (id, _) -> id) 
    
    // all available chars on hand
    match (stepWord word state.dict) with 
    | None -> emptyMoveDto
    | Some (_, d) -> 
      List.map (fun h -> (0, h)) handIds 
      |> fun h -> loopHand state.hand 0 d h emptyMoveDto

  // Convert the input moveDto to a move
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
    let ev1 = evalMove state (toMove state mov m1)
    let ev2 = evalMove state (toMove state mov m2)
    if ev1 > ev2 then m1 else m2

  let bestMove (state: stateDto) : move = 
    let maxfun = max state
    let bestext = bestExtension state

    let getBest (h: Hook) (cur: moveDto) : moveDto =
      let next = bestext h.word (maxfun h.mov)
      maxfun (h.mov) cur next

    // Run max_move recursively by applying backtracking
    let rec aux (hooks: Hook list) (best: moveDto) : move =
      match hooks with
      | h :: [] ->
        getBest h best |> (toMove state h.mov)
      | h :: hs ->
        getBest h best |> aux hs
      | [] -> emptyMove // If no more words to check in this branch, just return empty move

    aux state.hooks emptyMoveDto

  // stateDto => hook : <Movement, word> 
  // let getMovements (words: word list) : () list 

  /// Returns true if `mv` would be valid to play on the current state  
  // let isValid (mv:move) : bool = failwith "Not implemented IsValid"
  