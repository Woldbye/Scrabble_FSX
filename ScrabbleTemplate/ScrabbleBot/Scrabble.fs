namespace bufiobot

open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO

open ScrabbleUtil.DebugPrint

open MoveGen
open Entities

// The RegEx module is only used to parse human input. It is not used for the final product.

module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?" 
        Regex.Matches(ts, pattern) |>
        Seq.cast<Match> |> 
        Seq.map 
            (fun t -> 
                match t.Value with
                | Regex pattern [x; y; id; c; p] ->
                    ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)") |>
        Seq.toList

module Print =

    let printHand pieces hand =
        hand |>
        MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State = 
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.
    
    type state = {
        board         : Parser.board
        dict          : Dictionary.Dict
        playerNumber  : uint32
        playerTurn    : uint32
        nrOfPlayers   : uint32
        hand          : MultiSet.MultiSet<uint32>
        tiles         : Map<uint32, tile>
        bricks        : Map<coord, tile>
        hooks         : Hook list // TODO: Fix hooks
        turns         : uint32
        // TO:DO add player number
    }

    // TODO:
    // Loop through all bricks
    // Find all hooks
    //    create helper functions for easier time (next on movement, isSquareOccupied, ...)
    // Find #chars legal to place
    // Update states
    // Send / recieve correct messages


    let mkState b d pn pt nop h t hs r ts = {
      board = b;
      dict = d;
      playerNumber = pn;
      playerTurn = pt;
      nrOfPlayers = nop;
      hand = h;
      tiles = t;
      hooks = hs;
      bricks = r;
      turns = ts;
    }

    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let playerTurn st    = st.playerTurn
    let nrOfPlayers st   = st.nrOfPlayers
    let hand st          = st.hand
    let tiles st         = st.tiles
    let bricks st        = st.bricks
    let hooks st         = st.hooks
    let turns st         = st.turns


    let nextTurn st =
        let n = (st.playerTurn + 1u) % st.nrOfPlayers
        match n with
        | 0u -> st.nrOfPlayers
        | m  -> m

    let toStateDto (s: state) : stateDto =
      {
        board         = s |> board
        dict          = s |> dict
        playerNumber  = s |> playerNumber
        playerTurn    = s |> playerTurn
        hand          = s |> hand
        tiles         = s |> tiles
        hooks         = s |> hooks
        bricks        = s |> bricks
        turns         = s |> turns
      }


//! TODO: Actually use nr of legal tiles
module Scrabble =
    open System.Threading
    open Bufio
    open MoveGen

    let playGame cstream pieces (st : State.state) =
        
        let rec aux (st : State.state) : unit =
            printf "Nr of tiles: %d\n" st.bricks.Count 
            //Print.printHand pieces (State.hand st)
            //forcePrint (sprintf "Player Turn: %d \n" st.playerTurn)

            match st.playerTurn with
            | p when p = st.playerNumber ->
                //debugPrint "Finding move...\n"
                let move = st |> State.toStateDto |> nextMove
                //debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
                send cstream (SMPlay move)
            | _ -> ()

            let msg = recv cstream
            
            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                
                let removeHandPieces hand =
                    List.fold (fun s (_, (t, _)) -> MultiSet.removeSingle t s) hand ms
                let addHandPieces hand =
                    List.fold (fun acc (x, k) -> MultiSet.add x k acc) hand newPieces
                    
                let brs = moveToBricksMap st.bricks ms
                let st' : State.state = {
                    board         = st.board
                    dict          = st.dict
                    playerNumber  = st.playerNumber
                    playerTurn    = (State.nextTurn st)
                    nrOfPlayers   = st.nrOfPlayers
                    hand          = (st.hand |> removeHandPieces |> addHandPieces)
                    tiles         = st.tiles
                    hooks         = (brs |> bricksToHooks)
                    bricks        = brs
                    turns         = st.turns + 1u
                }
                //printf "New hand: %A\n" st'.hand
                aux st'
                
            | RCM (CMPlayed (pid, ms, points)) ->
                let brs = moveToBricksMap st.bricks ms
                let st' : State.state = {
                  board         = st.board
                  dict          = st.dict
                  playerNumber  = st.playerNumber // Increment playerNumber
                  playerTurn    = State.nextTurn st
                  nrOfPlayers   = st.nrOfPlayers
                  hand          = st.hand  // clean hand such that move removes chars, and append newPieces
                  tiles         = st.tiles // Dont touch :)
                  hooks         = brs |> bricksToHooks
                  bricks        = brs
                  turns         = st.turns + 1u
                }

                aux st'
                
            | RCM (CMPlayFailed (pid, ms)) ->
                debugPrint (sprintf "Id: %d\n\tMS: %A\n\t" pid ms)
                // (* Failed play. Update your state *)
                // let st' : State.state = {
                //   board         = st |> State.board
                //   dict          = st |> State.dict
                //   playerNumber  = st |> State.playerNumber
                //   playerTurn    = st |> State.playerTurn
                //   nrOfPlayers   = st |> State.nrOfPlayers
                //   hand          = st |> State.hand
                //   tiles         = st |> State.tiles
                //   hooks         = st |> State.hooks
                //   bricks        = st |> State.bricks
                // }
                printf "oh no an error - passing \n"
                send cstream SMPass
                aux st
            | RCM (CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st

        aux st
        forcePrint "Completed!"

    let startGame 
            (boardP : boardProg) 
            (dictf : bool -> Dictionary.Dict) 
            (numPlayers : uint32) 
            (playerNumber : uint32) 
            (playerTurn  : uint32) 
            (hand : (uint32 * uint32) list)
            (tiles : Map<uint32, tile>)
            (timeout : uint32 option) 
            (cstream : Stream) =
        debugPrint 
            (sprintf "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n" numPlayers playerNumber playerTurn hand timeout)

        //let dict = dictf true // Uncomment if using a gaddag for your dictionary
        let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.mkBoard boardP
                  
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber playerTurn numPlayers handSet tiles [] Map.empty 0u)
        