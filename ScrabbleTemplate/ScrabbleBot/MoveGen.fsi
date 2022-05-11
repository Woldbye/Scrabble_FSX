module internal MoveGen
    open ScrabbleUtil
    open Eval
    open Parser
    open StateMonad
    open Entities

    type moveDto = uint32 list
    val bestMove : stateDto -> Entities.move
    val bestExtension : stateDto -> word -> (moveDto -> moveDto -> moveDto) -> moveDto
    val max : stateDto -> Entities.move -> Entities.move -> Entities.move 
    val stepWord : word -> Dictionary.Dict -> option<bool * Dictionary.Dict>
    val evalMove : stateDto -> move -> int
    val getMinTile : state: stateDto -> cid: uint32 -> (char * int)