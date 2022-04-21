module internal Eval

    open StateMonad

    (* Code for testing *)
    let hello = [('H', 4); ('E', 1); ('L', 1); ('L', 1); ('O', 1)] 
    let state = mkState [("x", 5); ("y", 42)] hello ["_pos_"; "_result_"]
    let emptyState = mkState [] [] []
    
    let oneOp f a = a >>= fun x -> x |> f |> ret

    let twoOp f a b =
        a >>= fun x ->
        b >>= fun y ->
        (x, y) ||> f |> ret

    let twoNDZOp f a b =
        a >>= fun x ->
        b >>= fun y ->
            match y with
            | 0 -> fail DivisionByZero
            | _ -> (x, y) ||> f |> ret

    let add = twoOp (+)
    let sub = twoOp (-)
    let mul = twoOp (*)
    let div = twoNDZOp (/)
    let modS = twoNDZOp (%)
    let isVowel ch =
        ch
        |> System.Char.ToLower 
        |> fun t -> List.exists (fun v -> v = t ) [ 'a'; 'e'; 'i'; 'o'; 'u'; 'y' ]


    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
        | Div of aExp * aExp
        | Mod of aExp * aExp
        | CharToInt of cExp

    and cExp =
       | C  of char  (* Character value *)
       | CV of aExp  (* Character lookup at word index *)
       | ToUpper of cExp
       | ToLower of cExp
       | IntToChar of aExp

    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)

       | IsVowel of cExp      (* check for vowel *)
       | IsLetter of cExp     (* check for letter *)
       | IsDigit of cExp      (* check for digit *)
    
        
    type stm =                    (* statements *)
        | Declare of string       (* variable declaration *)
        | Ass of string * aExp    (* variable assignment *)
        | Skip                    (* nop *)
        | Seq of stm * stm        (* sequential composition *)
        | ITE of bExp * stm * stm (* if-then-else statement *)
        | While of bExp * stm     (* while statement *)


    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *) 
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)    
    
    let singleEval ev x f = x |> ev |> f
    let toupleEval ev x y f = (x |> ev, y |> ev) ||> f

    let rec arithEval a : SM<int> =    
        let atEval = toupleEval arithEval
        match a with
        | N n         -> n |> ret
        | V v         -> v |> lookup
        | WL          -> wordLength
        | PV pv       -> pv |> arithEval >>= pointValue
        | Add (x, y)  -> atEval x y add
        | Sub (x, y)  -> atEval x y sub
        | Mul (x, y)  -> atEval x y mul
        | Div (x, y)  -> atEval x y div
        | Mod (x, y)  -> atEval x y modS
        | CharToInt c -> c |> charEval >>= fun x -> x |> int |> ret

    and charEval c : SM<char> =
        match c with
        | C c   -> c |> ret
        | CV cv -> cv |> arithEval >>= characterValue
        | ToUpper c -> c |> charEval >>= fun x -> x |> System.Char.ToUpper |> ret
        | ToLower c -> c |> charEval >>= fun x -> x |> System.Char.ToLower |> ret
        | IntToChar i -> i |> arithEval >>= fun x -> x |> char |> ret

    /// <summary>
    /// Evaluate a bExp to a boolean state monad
    /// </summary>
    /// <param name="b"> A bExp to evaluate </param>
    /// <returns> A boolean state monad </returns>
    and boolEval (b: bExp) : SM<bool> =
        let atEval = toupleEval arithEval
        let btEval = toupleEval boolEval
        
        match b with
        | TT          -> true   |> ret
        | FF          -> false  |> ret
        | AEq (a, b)  -> atEval a b (twoOp (=)) 
        | ALt (a, b)  -> atEval a b (twoOp (<))
        | Not bx      -> singleEval boolEval bx (oneOp (not))
        | Conj (a, b) -> btEval a b (twoOp (&&))  
        | IsVowel c   -> singleEval charEval c (oneOp isVowel)
        | IsLetter c  -> singleEval charEval c (oneOp System.Char.IsLetter) 
        | IsDigit c   -> singleEval charEval c (oneOp System.Char.IsDigit) 

    and stmntEval (stmnt: stm) : SM<unit> = 
        let branch st = push >>>= (stmntEval st) >>>= pop
        
        match stmnt with
        | Declare s       -> s |> declare                               // Declare variable
        | Ass (str, a)    -> arithEval a >>= fun v -> (update str v)    // Update variable
        | Skip            -> () |> ret                                  // noOp
        | Seq (s1, s2)    -> stmntEval s1 >>>= stmntEval s2             // Seperate into sequences
        | ITE (b, s1, s2) -> boolEval b >>= (fun o -> (if o then s1 else s2) |> branch)
        | While (b, s)    -> 
            boolEval b
            >>= function
                | true ->
                    s |> branch
                    >>>= ((b, s) |> While |> stmntEval)
                | false -> Skip |> stmntEval


    (* Part 3 (Optional) *)
    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)
        
    let prog = new StateBuilder()

    (* Part 4 (Optional) *) 
    type word = (char * int) list
    type squareFun = word -> int -> int -> int

    // Returns a function that: 
    // Given a word, position and an acc
    let stmntToSquareFun (statement: stm): squareFun =
        fun w pos acc ->
            let vars = [("_pos_", pos); ("_acc_", acc); ("_result_", 0)]
            let rest = ["_pos_"; "_acc_"; "_result_"]
            let state = mkState vars w rest
            
            stmntEval statement
            >>>= lookup "_result_" 
            |> evalSM state
            |> function 
            | Success s   -> s
            | Failure _   -> 0
    
    type coord = int * int

    type boardFun = coord -> Result<squareFun option, Error> 

    // Create a function stmntToBoardFun : stmnt -> Map<int, squareFun> -> boardFun that given a
    // statement stm and a a lookup table of identifiers and square functions squares runs stm in the state
    // where:
    // The variable environment contains the variables _x_ , _y_ , and _result_ where:
    // the variables _x_ and _y_ are initialised to be the x- and the y-values of the coordinate to the
    // board function
    // the variable _result_ is initialised to 0
    // The word is set to the empty word (we guarantee it will never be used when evaluating boards)
    // _x_ , _y_ , and _result_ are reserved words.
    // after which it looks up the integer value id stored in _result_ and returns
    // Success (Some sf) , if looking up the key id in squares results in sf
    // Success None if the key id does not exist in squares
    // Fails if the evaluation of stm fails.
    // It is important to note that a result of Success None is not considered a failure, it just means that the
    // coordinate is empty (outside the board, for instance).
    
    let stmntToBoardFun (stm:stm) (m:Map<int, squareFun>) = 
        fun (x, y) ->
            let vars = [("_x_", x); ("_y_", y); ("_result_", 0)]
            let rest = ["_x_"; "_y_"; "_result_"]
            let state = mkState vars [] rest

            stmntEval stm
            >>>= lookup "_result_"
            >>= fun t -> ret (Map.tryFind t m)
            |> evalSM state

    type board = {
        center        : coord
        defaultSquare : squareFun
        squares       : boardFun
    }

    //! TO:DO 5
    // let mkBoard c defaultSq boardStmnt ids =    