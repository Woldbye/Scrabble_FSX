module internal MoveGen
  open ScrabbleUtil
  open Eval
  open Parser
  open StateMonad
  open Entities
  open System

    
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
  
  let moveToBricksMap (bricks: Map<coord, tile>) (mv: move) : Map<coord, tile> =
    //printf "Here it is : %A\n" mv
    List.fold (fun s (c, (_, t)) -> Map.add c (Set.add t Set.empty) s) bricks mv

  let isSquareOccupied (bricks: Map<coord, tile>) (c: coord) : bool =
    Map.containsKey c bricks

  let legalCharCount (bricks: Map<coord, tile>) (mm: Movement) : int =
    let isAt c = isSquareOccupied bricks c
    let rec aux m acc =
      
      let ic = m |> getPos |> isAt
      
      let (p0, p1) =  m |> getPerpCoords
      let ip0 = p0 |> isAt
      let ip1 = p1 |> isAt

      match (ic || ip0 || ip1 || (acc > 5)) with
      | true  -> acc
      | false -> aux (nextPos m) (acc + 1)

    // If a brick is already place behind, it is an illegal move
    let ic = mm |> prevPos |> getPos |> isAt
    match ic with 
    | true  -> -1
    | false -> aux (nextPos mm) -1

  let bricksToHooks (bricks: Map<coord, tile>) : Hook list =
    let rec aux bs dir acc : Hook list = 
      match bs with
      | (c : coord, t : tile) :: bbs ->
        let mv = mkMovement c dir
        let c = legalCharCount bricks mv
        let h = mkHook mv [t.MinimumElement] c
        aux bbs dir ([h] @ acc)
      | []            -> acc

    let blist: (coord * tile) list = bricks |> Map.toList
    let baux = blist |> aux
    
    let ds = baux Down List.empty
    let rs = baux Right List.empty
    let ts = ds @ rs
    List.filter (fun h -> h.count > 0) ts
    



  /// Returns number of points given for playing `mv` on `state`
  let evalMove (st: stateDto) (mv:move) : int =
    let folder (acc: (square list) option) (c, _) : ((square list) option) =
      match (st.board.squares c) with
        | Success sqOpt when (sqOpt.IsSome && acc.IsSome)
            ->
            match (st.bricks.ContainsKey c) with
            | true  -> None
            | false -> Some (acc.Value @ [sqOpt.Value])
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
  let stepWord (wrd: word) (d:Dictionary.Dict) : option<bool * Dictionary.Dict> = 
    let rec aux (w: word) (res:option<(bool * Dictionary.Dict)>) : option<bool * Dictionary.Dict> =
      match (w, res) with
      | ((c, _) :: ws, Some (_, next)) ->
        //printf "stepping into %c\n" c
        let nres = Dictionary.step c next
        aux ws nres
      | ([], Some r) -> Some r
      | (_, None)    -> None

    aux wrd (Some (false, d))


  // MAX_END_POINT
  // START POINT -> END POINT
  let getMinTile (state: stateDto) (cid: uint32): (char * int) = 
    state.tiles.[cid] |> fun t -> t.MinimumElement

  let getMinChar st i = getMinTile st i |> fun (c, _) -> c

  /// <returns> 
  /// Best scoring playable move that extends `word`, 
  /// if no extension available `emptyMove` 
  /// <returns /> 
  let bestExtension (state: stateDto) (word:word) (maxMove: moveDto -> moveDto -> moveDto) : moveDto =
    // TECHNIQUE: BACKTRACKING:
    // GOAL: RUN ALL POSSIBLE EXTENSIONS OF INPUT WORD
    let rec loopHand (hand: MultiSet.MultiSet<uint32>) (depth: int) (dict:Dictionary.Dict) (cids:(int * uint32) list) (acc:moveDto) : moveDto =
      // printf "Depth of: %d \n" depth
      // printf "With: %A \n" cids
      match cids with 
      | []                         -> emptyMoveDto // end recursion
      | (d, _) :: _ when d > depth -> emptyMoveDto
      | (_, c) :: tail when MultiSet.contains c hand = false -> loopHand hand depth dict tail acc
      | (d, cid) :: tail -> // cid = char id
        // Skip branch
        let m1 : moveDto = loopHand hand depth dict (tail @ [(d + 1, cid)]) acc

        // Remove played tile from hand
        let hand' = MultiSet.removeSingle cid hand;
        
        // extract char from tile
        let key : char = getMinTile state cid |> fun (c', _) -> c'
        //printf "Char is ??? %c\n" key
        // Update move
        let mv' : moveDto = acc @ [cid] 

        // Try play char
        let recHandLoop dd = loopHand hand' (depth + 1) dd cids mv'

        let m2 : moveDto = 
          match (Dictionary.step key dict) with 
          // No finished word, check sub-branches
          | Some (false, d') -> recHandLoop d'
          | Some (true, d')  -> recHandLoop d' |> maxMove mv'
          // No possible play in sub-branch 
          | None -> emptyMoveDto
        maxMove m1 m2
    // Ids -> state.tiles.[id]
    let handIds =
      MultiSet.toList state.hand |> List.map (fun (id, _) -> id)
      |> List.map (fun h -> (0, h))
    
    //printf "Start word: %A \n" word
    // all available chars on hand
    let init = List.fold (fun s (v, _) ->
        match s with
        | Some (_, d) -> Dictionary.step v d
        | None        -> None) (Some (false, state.dict)) word

    match init with 
    | Some (_, d) -> loopHand state.hand 0 d handIds emptyMoveDto
    | None -> emptyMoveDto


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

  let max (state: stateDto) (mov1: Movement) (mov2: Movement) (m1: moveDto) (m2:moveDto) : (Movement * moveDto) =
    let ev1 = evalMove state (toMove state mov1 m1)
    let ev2 = evalMove state (toMove state mov2 m2)
    if ev1 > ev2 then (mov1, m1) else (mov2, m2)

  let firstMoveHooks (state: stateDto) : Hook list =
    let getDefaultHook dir =
      let mm : Movement = {
        pos = state.board.center
        dir = dir
      }
      {
        mov = mm |> prevPos
        word = []
        count = state.hand |> MultiSet.size |> int
      }

    [ getDefaultHook Right; getDefaultHook Down ]

  let bestMove (state: stateDto) : move = 
    let maxfun = max state
    let bestext = bestExtension state

    let getBest (h: Hook) (cur: moveDto) (m: Movement) : (Movement * moveDto) =
      let bmf a b = maxfun h.mov h.mov a b |> fun (_, d) -> d
      let next = bestext h.word bmf
      maxfun m h.mov cur next

    // Run max_move recursively by applying backtracking
    let rec aux (hooks: Hook list) (best: moveDto) (bestMm: Movement) : move =
      // printf "Best for now is %A \n" best
      match hooks with
      | h :: [] -> getBest h best bestMm |> fun (m, d) -> toMove state m d
      | h :: hs -> getBest h best bestMm |> fun (m, d) -> aux hs d m 
      | [] -> emptyMove // If no more words to check in this branch, just return empty move

    match state.hooks with
    | []  -> state |> firstMoveHooks
    | hks -> hks
    |> fun hks -> aux hks emptyMoveDto hks.Head.mov