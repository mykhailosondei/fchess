module fchess.CLI.Console

open System
open fchess.CLI.CommandProcessor
open fchess.Core

let run () =
    printfn "Welcome to fchess Engine!"
    printfn "Type 'help' for available commands."

    let mutable gameState = Game.init()  // Some initial game state

    let rec loop () =
        printf "> "
        let input = Console.ReadLine()
        match input with
        | "quit" -> printfn "Goodbye!"
        | "help" ->
            printfn "Available commands: position, go, d, quit, ..."
            loop ()
        | cmd ->
            let processor = CommandProcessor()
            let newState = CommandHandler.handle processor (gameState, cmd)
            match newState with
            | Ok result -> gameState <- result
            | Error msg -> printfn $"%s{msg}"
            loop ()
    loop ()