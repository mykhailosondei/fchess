module fchess.CLI.Console

open System
open fchess.CLI.CommandProcessor
open fchess.CLI.Commands.ICommandStrategy
open fchess.Core.Utils

let run () =
    printfn "Welcome to fchess Engine!"
    printfn "Type 'help' for available commands."

    let processor = CommandProcessor()
    
    let rec loop () =
        printf "> "
        let input = Console.ReadLine()
        match input with
        | "quit" -> printfn "Goodbye!"
        | "help" ->
            printfn "Available commands: position, move, d, quit, moves, ..."
            loop ()
        | cmd ->
            processor.DefaultStrategy <- Some (
                { new ICommandStrategy with
                    member _.Execute currentState _args =
                        printfn "Command is not available."
                        printfn "Available commands:"
                        for command in processor.strategies.Keys do
                            log (createLogEntry Debug "Available command" command)
                            
                        Result.Ok currentState
                })
            
            let newState = processor.ProcessCommand(cmd)
            match newState with
            | Ok result ->
                processor.state <- result
            loop ()
    loop ()