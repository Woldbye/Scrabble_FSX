module internal Bufio
    open ScrabbleUtil
    open Eval

    type stateDto = {
        board         : Parser.board
        dict          : Dictionary.Dict
        playerNumber  : uint32
        hand          : MultiSet.MultiSet<uint32>
        // TO:DO add player number
    }

    type move = list<coord * (uint32 * (char * int))>

    let nextMove (state: stateDto) : move = failwith "Not implemented nextMove" 
            // rec maximize stateDto : move  
            // ---> 
            // |
            // DOWN
            // WORDS ON BOARD 
            // SME WORD ON BOARD: ABE
            // HAND: R N E
            // => 
            // "A" : "B" : "E" -> "R" -> "N" -> ""
            //    HE 
            // ABE
            //  

    let isValid (mv:move) : bool = failwith "Not implemented IsValid"