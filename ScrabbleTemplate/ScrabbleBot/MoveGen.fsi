module internal MoveGen
    open ScrabbleUtil
    open Eval
    open Parser
    open StateMonad
    open Entities
    open System.Threading.Tasks

    type moveDto = uint32 list
    val bestMove : stateDto -> Entities.move
    val stepWord : word -> Dictionary.Dict -> option<bool * Dictionary.Dict>
    val evalMove : stateDto -> move -> int
    val getMinTile : state: stateDto -> cid: uint32 -> (char * int)
    val calculatePoints : square list -> tiles:word -> int
    val moveToWord : mv: move -> word
    val bricksToHooks : Map<coord, tile> -> Hook list
    val legalCharCount : Map<coord, tile> -> Movement -> int
    val isSquareOccupied : Map<coord, tile> -> coord -> bool
    val moveToBricksMap : Map<coord, tile> -> move -> Map<coord, tile>
