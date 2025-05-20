module fchess.CLI.CommandHandler

open fchess.Core.Game
open fchess.CLI.CommandProcessor
open fchess.CLI.Commands.ICommandStrategy
        
let handle (processor: CommandProcessor) (state: Game, cmd: string) =
    processor.ProcessCommand(state, cmd)

let handleWithObjectExpression (state: Game, cmdInput: string) =
    let parts = cmdInput.Trim().Split([|' '|], 2, System.StringSplitOptions.RemoveEmptyEntries)
    let commandName = if parts.Length > 0 then parts.[0] else ""

    if commandName = "custom_echo" then
        let customEchoStrategy = 
            { new ICommandStrategy with
                member _.Execute(currentState, args) =
                    printfn $"Echoing from object expression: {args}"
                    Result.Ok currentState
            }
        customEchoStrategy.Execute(state, cmdInput)
    else
        Result.Error("This handler only supports 'custom_echo' via object expression.")

        
