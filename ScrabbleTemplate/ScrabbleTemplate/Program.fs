﻿
// Learn more about F# at http://fsharp.org
open System
open System.Threading;
open bufiobot


let time f =
    let start = System.DateTime.Now
    let res = f ()
    let finish = System.DateTime.Now
    (res, finish - start)

let readLines filePath = System.IO.File.ReadLines(filePath)

let spawnMultiples name dict bot =
    let rec aux =
        function 
        | 0 -> []
        | x -> (sprintf "%s%d" name x, dict, bot)::aux (x - 1)
   
    aux >> List.rev

[<EntryPoint>]
let main argv =
    ScrabbleUtil.DebugPrint.toggleDebugPrint false // Change to false to supress debug output

    System.Console.BackgroundColor <- System.ConsoleColor.White
    System.Console.ForegroundColor <- System.ConsoleColor.Black
    System.Console.Clear()


    //let board        = ScrabbleUtil.StandardBoard.standardBoard ()
    let board      = ScrabbleUtil.InfiniteBoard.infiniteBoard ()

    //    let board      = ScrabbleUtil.RandomBoard.randomBoard ()
    //let board      = ScrabbleUtil.RandomBoard.randomBoardSeed (Some 42)
    //let board      = ScrabbleUtil.InfiniteRandomBoard.infiniteRandomBoard ()
    //let board      = ScrabbleUtil.InfiniteRandomBoard.infiniteRandomBoardSeed (Some 42)

    //    let board      = ScrabbleUtil.HoleBoard.holeBoard ()
    //    let board      = ScrabbleUtil.InfiniteHoleBoard.infiniteHoleBoard ()

    let words     = readLines "ScrabbleTemplate/Dictionaries/English.txt"

    let handSize   = 7u
    let timeout    = None
    let tiles      = ScrabbleUtil.English.tiles 1u
    let seed       = None
    let port       = 13001

    let dictAPI: Dictionary.Dict ScrabbleUtil.Dictionary.dictAPI option =
        Some (Dictionary.empty, Dictionary.insert, Dictionary.step, None) 

    let (dictionary, time) =
        time (fun () -> ScrabbleUtil.Dictionary.mkDict words dictAPI)

    // Uncomment this line to call your client
    let players = // spawnMultiples "Bufiobot" dictionary Oxyphenbutazone.Scrabble.startGame 2
      [
        ("Bufiobot", dictionary, bufiobot.Scrabble.startGame);
        ("Bufiobot", dictionary, bufiobot.Scrabble.startGame);
        ("OxyphenButazone", dictionary, Oxyphenbutazone.Scrabble.startGame)
      ]
    // let players = spawnMultiples "OxyphenButazone" dictionary Oxyphenbutazone.Scrabble.startGame 2
    
    // Dictionary Test
    // let incorrectWords = ScrabbleUtil.Dictionary.test words 10000 (dictionary false)
    // for s in incorrectWords do ScrabbleUtil.DebugPrint.forcePrint (s + "\n")


    // change to true if using a GADDAG

    do ScrabbleServer.Comm.startGame 
        board dictionary handSize timeout tiles seed port players
    
    ScrabbleUtil.DebugPrint.forcePrint ("Server has terminated. Press Enter to exit program.\n")
    System.Console.ReadLine () |> ignore
    

    0
