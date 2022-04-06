module internal StateMonad

    type Error = 
        | VarExists of string
        | VarNotFound of string
        | IndexOutOfBounds of int
        | DivisionByZero 
        | ReservedName of string  
        | EmptyStack of string

    type Result<'a, 'b>  =
        | Success of 'a
        | Failure of 'b

    type State = { vars     : Map<string, int> list
                   word     : (char * int) list 
                   reserved : Set<string> }

    type SM<'a> = S of (State -> Result<'a * State, Error>)

    let mkState lst word reserved = 
           { vars = [Map.ofList lst];
             word = word;
             reserved = Set.ofList reserved }

    let evalSM (s : State) (S a : SM<'a>) : Result<'a, Error> =
        match a s with
        | Success (result, _) -> Success result
        | Failure error -> Failure error

    let bind (f : 'a -> SM<'b>) (S a : SM<'a>) : SM<'b> =
        S (fun s ->
              match a s with
              | Success (b, s') -> 
                match f b with 
                | S g -> g s'
              | Failure err     -> Failure err)


    let ret (v : 'a) : SM<'a> = S (fun s -> Success (v, s))
    let fail err     : SM<'a> = S (fun s -> Failure err)

    let (>>=)  x f = bind f x
    let (>>>=) x f = x >>= (fun () -> f)

    let push : SM<unit> = 
        S (fun s -> Success ((), {s with vars = Map.empty :: s.vars}))

    let pop : SM<unit> =
        S (fun s ->
            match s.vars with
            | _ :: tail -> Success ((), { s with vars = tail })
            | []        -> "Pop on empty stack not allowed" |> EmptyStack |> Failure   
        )

    let wordLength : SM<int> =
        let rec aux acc =
            function
            | []      -> acc
            | _ :: cs -> aux (acc + 1) cs
        S (fun s -> Success (aux 0 s.word, s))

    let fromTouple pos fetch =
        let aux s w =
            let rec auxRec word i =
                match (i, word) with
                | (_, [])                  -> pos |> IndexOutOfBounds |> fail
                | (i, t :: _) when i = pos -> t |> fetch |> fun v -> (v, s) |> ret
                | (i, _ :: word)           -> auxRec word (i+1)
            auxRec w 0
        S (fun s -> evalSM s (aux s s.word))

    let characterValue (pos : int) : SM<char> = fromTouple pos (fun (c, _) -> c)
    let pointValue (pos : int) : SM<int> = fromTouple pos (fun (_, v) -> v)
    
    // Find variable x in stack and apply f
    let varFind f (x: string) : SM<'a> =
        let rec aux =
            function
            | []      -> None
            | m :: ms -> 
                match Map.tryFind x m with
                | Some v -> Some v
                | None   -> aux ms
        S (fun s -> f s (aux s.vars))

    // Checks if var is in variables
    let lookup (x : string) : SM<int> = 
        let aux s = function
            | Some v -> Success (v, s)
            | None   -> Failure (VarNotFound x)
        varFind aux x
    
    let reservedContains (x : string) : SM<unit> = 
        S (fun s -> 
            match (Set.contains x s.reserved) with
            | true  -> Failure (ReservedName x)
            | false -> Success ((), s))

    let declare (x : string) : SM<unit> =
        let aux s = function // isFree
            | Some _ -> Failure (VarExists x)
            | _      -> Success ((), s)

        (varFind aux x)           // check if declared
        >>>= (reservedContains x) // check if reserved
        >>>= S (fun s ->
            match s.vars with
            | top :: tail -> Success ((), {s with vars = top.Add(x, 0) :: tail})
            | _           -> "Can not get first top of empty stack" |> EmptyStack |> Failure
        )
    
    let update (x : string) (value : int) : SM<unit> =
        let rec auxChange vars = 
            match vars with
            | [] -> [] // never occours
            | curr :: tail -> 
                match (Map.tryFind x curr) with
                | Some _ -> (Map.change x (fun _ -> Some value) curr) :: tail
                | None   -> curr :: (auxChange tail)
            
        lookup x // Check that x exists
        >>= fun _ -> S ( fun s -> Success ((), { s with vars = auxChange s.vars }) ) // Update value
        
    
    