module internal Parser

    open Eval
    open ScrabbleUtil
    open FParsecLight

    (*
    The interfaces for JParsec and FParsecLight are identical and the implementations should always produce the same output
    for successful parses although running times and error messages will differ. Please report any inconsistencies.
    *)

    open FParsecLight.TextParser
    
    let pIntToChar  = pstring "intToChar"
    let pPointValue = pstring "pointValue"

    let pCharToInt  = pstring "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charValue"

    let pTrue     = pstring "true"
    let pFalse    = pstring "false"
    let pIsDigit  = pstring "isDigit"
    let pIsLetter = pstring "isLetter"
    let pIsVowel  = pstring "isVowel"

    let pif       = pstring "if"
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"
    let pdeclare  = pstring "declare"

    let whitespaceChar = satisfy System.Char.IsWhiteSpace <?> "whitespace"
    let pletter        = satisfy System.Char.IsLetter <?> "letter"
    let palphanumeric  = satisfy System.Char.IsLetterOrDigit <?> "alphanumeric"

    let spaces  = many whitespaceChar
    let spaces1 = many1 whitespaceChar <?> "space1"

    let (.>*>.) p1 p2 = (p1 .>> spaces) .>>. p2
    let (.>*>) p1 p2  = (p1 .>> spaces) .>> p2
    let (>*>.) p1 p2  = (p1 .>> spaces) >>. p2 

    let parenthesise p = pchar '(' >*>. p .>*> pchar ')'
    let brackets p     = pchar '{' >*>. p .>*> pchar '}'
    let singleQuotes p = pchar ''' >>. p .>> pchar '''

    let pid =
        (pletter <|> pchar '_')
        .>>. many (palphanumeric <|> pchar '_')
        |>> fun (x, y) -> List.fold (fun a b -> a + (string b)) (string x) y
    
    let unop op = (>*>.) op
    let binop op p1 p2 = (p1 .>*> op) .>*>. p2 

    // Arithmetic
    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()
    
    // Char
    let CharParse, cref = createParserForwardedToRef<cExp>()
    
    // Boolean
    let BSetParse, bsref = createParserForwardedToRef<bExp>()   // \/, /\
    let BCmpParse, bcref = createParserForwardedToRef<bExp>()   // =, <>, <, <=, >, >=
    let BAtomParse, baref = createParserForwardedToRef<bExp>()  // ~, isLetter, isVowel, isDigit
    
    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    let CharToIntParse = unop pCharToInt (parenthesise CharParse) |>> CharToInt <?> "CharToInt"
    do tref := choice [CharToIntParse; AddParse; SubParse; ProdParse]

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    do pref := choice [MulParse; DivParse; ModParse; AtomParse]

    let NParse   = pint32 |>> N <?> "Int"
    let ParParse = parenthesise TermParse
    let NegParse = unop (pchar '-') TermParse <?> "Mul" |>> (fun n -> Mul (N -1, n)) <?> "Mul"
    let VarParse = pid |>> V <?> "V"
    let PVParse  = unop pPointValue (parenthesise TermParse) |>> PV <?> "PV"
    do aref := choice [NegParse; PVParse; ParParse; NParse; VarParse]

    let AexpParse = TermParse 
    
    // Char parsing
    let CharAtomParse  = singleQuotes (whitespaceChar <|> palphanumeric) |>> C <?> "C"

    let CharValueParse = unop pCharValue (parenthesise TermParse) |>> CV <?> "CV"
    let ToUpperParse   = unop pToUpper (parenthesise CharParse) |>> ToUpper <?> "ToUpper"
    let ToLowerParse   = unop pToLower (parenthesise CharParse) |>> ToLower <?> "ToLower"
    let IntToCharParse = unop pIntToChar (parenthesise TermParse) |>> IntToChar <?> "IntToChar"

    do cref := choice [ ToUpperParse; ToLowerParse; IntToCharParse; CharValueParse; CharAtomParse ]

    // Boolean parser
    // TT          -> true   |> ret
    // FF          -> false  |> ret
    // AEq (a, b)  -> atEval a b (twoOp (=)) 
    // ALt (a, b)  -> atEval a b (twoOp (<))
    // Not bx      -> singleEval boolEval bx (oneOp (not))
    // Conj (a, b) -> btEval a b (twoOp (&&))  
    // IsVowel c   -> singleEval charEval c (oneOp isVowel)
    // IsLetter c  -> singleEval charEval c (oneOp System.Char.IsLetter) 
    // IsDigit c   -> singleEval charEval c (oneOp System.Char.IsDigit) 
    
    let ConjParse = binop (pstring "/\\") BCmpParse BSetParse |>> Conj <?> "Conj"
    let UnionParse = binop (pstring "\\/") BCmpParse BSetParse <?> "Conj"
                        |>> (fun (a, b) -> Conj (Not a, Not b)) <?> "Conj"
    
    do bsref := choice [ ConjParse; UnionParse; BCmpParse ]
     
    let AEqParse    = binop (pchar '=') AexpParse AexpParse |>> AEq <?> "AEq"
    let ANotEqParse = binop (pstring "<>") AexpParse AexpParse <?> "AEq"
                        |>> (fun (a, b) -> Not (AEq (a, b))) <?> "AEq"
    let ALtParse    = binop (pchar '<') AexpParse AexpParse |>> ALt <?> "ALt"
    let ALtEqParse  = binop (pstring "<=") AexpParse AexpParse
                        |>> (fun (a, b) ->  Conj (Not (ALt (a,b)), Not (AEq ((a, b)))))      
    let AGrtParse   = binop (pchar '>') AexpParse AexpParse 
                        |>> (fun (a, b) -> Not (Conj (Not (ALt (a,b)), Not (AEq ((a, b))))))
    let AGrEqParse  = binop (pstring ">=") AexpParse AexpParse 
                        |>> (fun (a, b) -> Not (ALt (a, b)))
    
    bcref := choice [ AEqParse; ANotEqParse; ALtParse; ALtEqParse; AGrtParse; AGrEqParse; BAtomParse ]
    
    let notParse      = unop (pchar '~') BAtomParse |>> Not <?> "Not"
    let isLetterParse = unop pIsLetter (parenthesise CharParse) |>> IsLetter <?> "IsLetter"
    let isVowelParse  = unop pIsVowel (parenthesise CharParse) |>> IsVowel <?> "IsVowel"
    let isDigitParse  = unop pIsDigit (parenthesise CharParse) |>> IsDigit <?> "IsDigit"

    let TrueParse  = pTrue |>> (fun _ -> TT) <?> "TT" 
    let FalseParse = pFalse |>> (fun _ -> FF) <?> "FF"
    
    do baref := choice [ notParse; isLetterParse; isVowelParse; isDigitParse; TrueParse; FalseParse ]

    let CexpParse = CharParse
    let BexpParse = BSetParse
    let stmntParse = pstring "not implemented"

    (* These five types will move out of this file once you start working on the project *)
    type word   = (char * int) list
    type square = Map<int, word -> int -> int -> int>
  
    type boardFun = coord -> square option
    // Given a square program, run the stmntParse on all source code 
    let parseSquareProg (sqp: squareProg): square = failwith "not implemented"
        
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun
    }

    let mkBoard (bp : boardProg) = failwith "not implemented"
       
    // let b = {
    //     center = bp.center;
    //     defaultSquare = None;
    //     squares = bp.squares;
    // }
    // b

    let parseBoardProg (bp: boardProg) : board = failwith "not implemented"
        //mkBoard bp

