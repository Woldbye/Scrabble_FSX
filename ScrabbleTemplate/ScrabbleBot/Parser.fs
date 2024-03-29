﻿module internal Parser

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
  let SeqParse, seqref = createParserForwardedToRef<stm>()
  let StmParse, sref = createParserForwardedToRef<stm>()
  
  let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
  let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
  do tref := choice [AddParse; SubParse; ProdParse]

  let CharParParse = parenthesise CharParse
  let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
  let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
  let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
  let CharToIntParse = unop pCharToInt CharParParse |>> CharToInt <?> "CharToInt"
  do pref := choice [MulParse; DivParse; ModParse; CharToIntParse; AtomParse]

  let NParse   = pint32 |>> N <?> "Int"
  let ParParse = parenthesise TermParse
  let NegParse = unop (pchar '-') TermParse <?> "Mul" |>> (fun n -> Mul (N -1, n)) <?> "Mul"
  let VarParse = pid |>> V <?> "V"
  let PVParse  = unop pPointValue ParParse|>> PV <?> "PV"
  do aref := choice [NegParse; PVParse; VarParse; NParse; ParParse]

  
  // Char parsing
  let CharAtomParse  = singleQuotes (anyChar <|> whitespaceChar) |>> C <?> "Char"

  let CharValueParse = pCharValue >*>. ParParse |>> CV <?> "CharValue"
  let ToUpperParse   = pToUpper >*>. CharParParse |>> ToUpper <?> "ToUpper"
  let ToLowerParse   = pToLower >*>. CharParParse |>> ToLower <?> "ToLower"
  let IntToCharParse = pIntToChar >*>. ParParse |>> IntToChar <?> "IntToChar"
  
  do cref := choice [ IntToCharParse; ToUpperParse; ToLowerParse; CharValueParse; CharParParse; CharAtomParse ]
  
  let BoolParParse = parenthesise BSetParse

  let ConjParse = binop (pstring "/\\") BCmpParse BSetParse |>> (fun (a, b) -> a .&&. b) <?> "Conj"
  let UnionParse = binop (pstring "\\/") BCmpParse BSetParse |>> (fun (a, b) -> a .||. b) <?> "Disj"
  
  do bsref := choice [ ConjParse; UnionParse; BCmpParse ]
    
  let AEqParse    = binop (pchar '=') TermParse TermParse |>> AEq <?> "AEq"
  let ANotEqParse = binop (pstring "<>") TermParse TermParse <?> "AEq"
                      |>> (fun (a, b) -> Not (AEq (a, b))) <?> "AEq"
  let ALtParse    = binop (pchar '<') TermParse TermParse |>> ALt <?> "ALt"
  let ALtEqParse  = binop (pstring "<=") TermParse TermParse
                      |>> (fun (a, b) ->  Conj (Not (ALt (a,b)), Not (AEq ((a, b)))))      
  let AGrtParse   = binop (pchar '>') TermParse TermParse 
                      |>> (fun (a, b) -> Not (Conj (Not (ALt (a,b)), Not (AEq ((a, b))))))
  let AGrEqParse  = binop (pstring ">=") TermParse TermParse 
                      |>> (fun (a, b) -> Not (ALt (a, b)))
  
  do bcref := choice [ AEqParse; ANotEqParse; ALtParse; ALtEqParse; AGrtParse; AGrEqParse; BAtomParse ]
  
  let notParse      = unop (pchar '~') BAtomParse |>> Not <?> "Not"
  let isLetterParse = unop pIsLetter (parenthesise CharParse) |>> IsLetter <?> "IsLetter"
  let isVowelParse  = unop pIsVowel (parenthesise CharParse) |>> IsVowel <?> "IsVowel"
  let isDigitParse  = unop pIsDigit (parenthesise CharParse) |>> IsDigit <?> "IsDigit"

  let TrueParse  = pTrue |>> (fun _ -> TT) <?> "TT" 
  let FalseParse = pFalse |>> (fun _ -> FF) <?> "FF"
  
  do baref := choice [ TrueParse; FalseParse; notParse; isLetterParse; isVowelParse; isDigitParse; BoolParParse; ]

  let seqParse = binop (pchar ';') StmParse SeqParse |>> Seq <?> "Seq"
  
  do seqref := choice [ seqParse; StmParse ]

  let declareParse = pdeclare |>> (fun s -> Declare s) <?> "Declare"
  let assParse     = binop (pstring ":=") pid TermParse |>> Ass <?> "Ass"
  let whileParse   = (unop pwhile (parenthesise BSetParse)) .>*>. 
                     (unop pthen (brackets StmParse)) |>> 
                     (fun (x,y) -> While (x, y)) <?> "While"

  let iTEParse  = (unop pif BoolParParse) .>*>.
                  (unop pthen (brackets StmParse)) .>*>.
                  (unop pelse (brackets StmParse)) |>>
                  (fun ((a, b), c) -> ITE (a, b, c)) <?> "ITE"

  let iFParse   = (unop pif BoolParParse) .>*>.
                  (unop pthen (brackets StmParse)) |>>
                  (fun (a, b) -> ITE (a, b, Skip)) <?> "IT"

                      
  do sref := choice [ iTEParse; iFParse; whileParse; declareParse; assParse ] // STM_PARSE

  let AexpParse = TermParse 
  let CexpParse = CharParse
  let BexpParse = BSetParse
  let stmntParse = SeqParse

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
  
  type boardFun2 = coord -> square option
  
  type board = {
      center        : coord
      defaultSquare : square
      squares       : boardFun2
  }
  
  // Parses input string @prog with @squares into a board function
  let parseBoardProg (prog:string) (squares:Map<int, square>) : boardFun2 =
    // let exec cmd = Map.map (fun _ v -> v |> cmd) 
    let stm1 = getSuccess(run stmntParse prog)  // stm
    stmntToBoardFun stm1 squares

  let mkBoard (bp: boardProg) : board =
    let sqMap = Map.map (fun _ -> parseSquareProg) bp.squares // Map<int, square>
    let squares = parseBoardProg bp.prog sqMap // boardFun2
    let defSq = sqMap.[bp.usedSquare] // square
    {
      center = bp.center;
      defaultSquare = defSq;
      squares = squares;
    }

    // fun _ -> {center = (0,0); defaultSquare = Map.empty; squares = fun _ -> Success (Some Map.empty)}
