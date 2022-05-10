module internal MoveGen
    open ScrabbleUtil
    open Eval
    open Parser
    open StateMonad
    open Entities

    val bestMove : stateDto -> Entities.move
    val bestExtension : stateDto -> word -> Entities.move 
    val max : stateDto -> Entities.move -> Entities.move -> Entities.move 
    val stepWord : word -> Dictionary.Dict -> option<bool * Dictionary.Dict>
    val evalMove : stateDto -> move -> int