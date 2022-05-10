
module internal Dictionary
  // Dict node implemented with Map<Char, Node> for Log(N) lookup
  type Dict = S of (bool * Map<char, Dict>) 
  
  // Return empty Dict
  let empty (_: unit) : Dict = S (false, Map.empty)

  // Lookup if dict contains s
  let lookup (s: string) (dict: Dict) : bool =
    let rec lookupChar (S (_, map)) (lst: char list) =
      match lst with
      | x :: xs ->
        match map.ContainsKey(x) with
        | true -> lookupChar map.[x] xs 
        | false -> false
      | [] -> true
    lookupChar dict (Seq.toList s)

  // Insert s into dict
  let insert (s:string) (dict: Dict) : Dict =
    
    let getDict (map: Map<char, Dict>) (c: char) : Dict =
      match map.ContainsKey(c) with
      | true -> map.[c] 
      | false -> empty()

    let rec addChar (S (isLeaf, map)) (lst: char list) : Dict =  
      match lst with
      | x :: xs -> 
        getDict map x
        |> fun d -> addChar d xs
        |> fun d -> Map.add x d map
        |> fun m -> 
          S (isLeaf, m)
      | [] ->
        S (true, map)
    addChar dict (Seq.toList s)


  let isWord (d: Dict): (bool * Dict) = 
    d |> fun (S(leaf, _)) -> (leaf, d)

  /// @returns if there is a path from dict to dict' 
  ///             Some(b, dict') 
  ///                 b = True if following char completes a word
  ///                 b = False otherwise
  ///          if there is no path from dict to dict'
  ///              None
  let step (c:char) (S (_, next)) : (bool * Dict) option =
    match next.TryFind(c) with
    | Some d -> d |> isWord |> Some
    | None -> None
  