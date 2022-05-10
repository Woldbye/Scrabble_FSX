module internal MoveGen
  open ScrabbleUtil
  open Eval
  open Parser
  open StateMonad
  open Entities

  // type move = list<coord * (uint32 * (char * int))>
  // assumes w not empty
  let prefix (w:word) : char = w.Head |> fun (c,v) -> c
  let emptyMove: move = []  
  /// Returns number of points given for playing `mv` on `state`
  //! TO:DO implement, use calculatePoints for implementation
  let evalMove (st: stateDto) (mv:move) : int =
    match mv with 
    | emptyMove -> 0 
    | _ -> 0 // calculate move

  let max (st: stateDto) (m1:move) (m2:move) = 
    if evalMove st m1 > evalMove st m2 then m1 else m2

  // Attempt to play hand on word
  // let extendWord (state:stateDto) (w:word)

  

  /// <returns>
  /// None if word is not contained in dict or word is empty
  /// Some d where d is outgoing node from following `w`
  /// <returns/>
  let stepWord (word: word) (d:Dictionary.Dict) : option<bool * Dictionary.Dict> = 
    let rec aux (w: word) (res:option<(bool * Dictionary.Dict)>) : option<bool * Dictionary.Dict> =
      match (res, w) with 
      | (None, _) -> res
      | (_, []) -> res   
      | (Some (_, next), _) -> 
        ((w |> prefix), next) 
        ||> Dictionary.step 
        |> aux w 

    match word with 
    | [] -> None
    | _ -> aux word.Tail (Dictionary.step (word |> prefix) d)


  /// <returns> 
  /// Best scoring playable move that extends `word`, 
  /// if no extension available `emptyMove` 
  /// <returns /> 
  let bestExtension (state: stateDto) (word:word) : move = 
    /// Add tile with `id` to `mv`
    let appendTile (mv:move) (id: uint32) = failwith "not implemented" //! TO:DO implement appendTile
    
    // Backtracking hand looping recursion
    let rec loop_hand (st1: stateDto) (h:uint32 list) (acc:move) : move =
      match h with 
      | [] -> acc // end recursion
      | cid :: tail -> // cid = char id
        let key : char = state.tiles.[cid] |> fun (c', v') -> c' // extract char from tile
        let st2 : stateDto = {
          board = st1.board; 
          dict = st1.dict |> Dictionary.step key; //! TO:DO add option handling, and evaluate move if st.dict.step Some(true,_)
          playerNumber = st1.playerNumber;
          hand = st1.hand |> MultiSet.removeSingle cid;
          tiles = st1.tiles; 
        }
        // Skip cid for now and try next char
        let m1 : move = loop_hand st1 (tail @ cid) acc //! TO:DO add stop for inifinity loop
        // Play cid
        let m2 : move = loop_hand st2 h (appendTile acc cid)
        max state m1 m2 

    let hand_ids = MultiSet.toList state.hand |> List.map (fun (id, _) -> id) 
    
    // all available chars on hand
    match (stepWord word state.dict) with 
    | None -> emptyMove
    | Some (_, d) -> 
      let st : stateDto = { 
        board = state.board; dict = d; hand = state.hand; playerNumber = state.playerNumber; tiles = state.tiles; }
      loop_hand st hand_ids emptyMove

  let bestMove (state: stateDto) : move = failwith "not implemented"
    /// Run max_move recursively by applying backtracking
    // //! word list input might be unneccessary
    // let rec aux (s:stateDto) (words:word list) : move =
    //   match words with
    //   | w::ws -> 
    //     let cur = max_word s w
    //     let next = aux s ws 
    //     max cur next
    //   | [] -> emptyMove // If no more words to check in this branch, just return empty move
    // //! here we should retrieve all possible words on the board that can be extended
    // let hooks: word list = [['_', 1]] 
    // aux state hooks

  /// Returns true if `mv` would be valid to play on the current state  
  //! TO:DO Implement
  let isValid (mv:move) : bool = failwith "Not implemented IsValid"
  