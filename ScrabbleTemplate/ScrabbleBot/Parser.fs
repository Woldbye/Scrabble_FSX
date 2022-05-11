module internal Parser

  open Eval
  open ScrabbleUtil
  open FParsecLight
  open StateMonad

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
  
  // Statement
  let StmParse, sref = createParserForwardedToRef<stm>()
  
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
  
  let declareParse = pdeclare |>> (fun s -> Declare s) <?> "Declare"
  let assParse     = binop (pstring ":=") pid AexpParse |>> (fun (x,y) -> Ass (x, y)) <?> "Ass"
  let seqParse     = binop (pchar ';') StmParse StmParse |>> (fun (x,y) -> Seq (x, y)) <?> "Seq"
  let whileParse   = (unop pwhile (parenthesise BSetParse)) .>*>. 
                      (unop pthen (brackets StmParse)) |>> 
                      (fun (x,y) -> While (x, y)) <?>
                      "While"
  
  
  // if and else parse
  let ifParse      = (unop pif (parenthesise BSetParse)) .>*>.
                      (unop pthen (brackets StmParse)) .>*>.
                      (opt (unop pelse (brackets StmParse))) |>>
                      (fun ((b, s), oe) -> ITE ( b, s,
                          match oe with 
                          | Some v -> v
                          | None   -> Skip
                      ))
    
  do sref := choice [ declareParse; assParse; seqParse; whileParse; ifParse ]

  let CexpParse = CharParse
  let BexpParse = BSetParse
  let stmntParse = StmParse

  (* These five types will move out of this file once you start working on the project *)
  type word   = (char * int) list
  // tile @id
  type square = Map<int, squareFun>
    
  // Given a square program, parse the program to a square
  let parseSquareProg (sqp: squareProg): square =
      let exec cmd = Map.map (fun _ v -> v |> cmd) 
      sqp
      |> exec (run stmntParse)
      |> exec getSuccess
      |> exec stmntToSquareFun
  
  type boardFun2 = coord -> Result<square option, Error>
  type board = {
      center        : coord
      defaultSquare : square
      squares       : boardFun2
  }
  
  // Parses input string @prog with @squares into a board function
  let parseBoardProg (prog:string) (squares:Map<int, square>) : boardFun2 =
    // let exec cmd = Map.map (fun _ v -> v |> cmd) 
    let stm1 = getSuccess(run stmntParse prog)  // stm
    let x = stmntToBoardFun stm1 squares
    fun c -> Success (x c)

  let mkBoard (bp: boardProg) : board =
    let sqMap = Map.map (fun _ -> parseSquareProg) bp.squares // Map<int, square>
    let squares = parseBoardProg bp.prog sqMap // boardFun2
    let defSq = sqMap.[bp.usedSquare] // square
    {
      center = bp.center;
      defaultSquare = defSq;
      squares = squares;
    }
