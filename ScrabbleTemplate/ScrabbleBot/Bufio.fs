module internal Bufio
  open ScrabbleUtil
  open Eval
  open Parser
  open StateMonad
  open Entities
  open MoveGen

  type stateDto = {
    board         : Parser.board
    dict          : Dictionary.Dict
    playerNumber  : uint32
    hand          : MultiSet.MultiSet<uint32>
    // TO:DO add player number
  }

  let nextMove (state: stateDto) : move = failwith "notimplemented" 
  
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
  