module internal Bufio
  open ScrabbleUtil
  open Parser
  open StateMonad
  open Entities

  let nextMove (state: stateDto) : move = MoveGen.bestMove state
