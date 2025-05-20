module fchess.CLI.CommandProcessor

open System.Collections.Generic
open fchess.CLI.Commands
open fchess.CLI.Commands.ICommandStrategy
open fchess.CLI.Commands.PositionCommand
open fchess.CLI.Commands.DisplayBoardCommand
open fchess.CLI.Commands.DisplayMovesCommand
open fchess.CLI.Commands.MakeMoveCommand
open fchess.Core
open fchess.Core.Game

type CommandProcessor =
    val mutable state: Game
    val strategies: Dictionary<string, ICommandStrategy>
    val mutable defaultStrategy: ICommandStrategy option

    new() =
        let strategies = Dictionary<string, ICommandStrategy>()
        strategies.Add("position", PositionCommand() :> ICommandStrategy)
        strategies.Add("d", DisplayBoardCommand() :> ICommandStrategy)
        strategies.Add("moves", DisplayMovesCommand() :> ICommandStrategy)
        strategies.Add("move", MakeMoveCommand() :> ICommandStrategy)
        { strategies = strategies; defaultStrategy = None; state = Game.init() }
        
    member this.DefaultStrategy
        with get() = this.defaultStrategy
        and set(strategy: ICommandStrategy option) = this.defaultStrategy <- strategy
        
    member private this.Execute(strategy : ICommandStrategy) =
        strategy.Execute this.state

    member this.ProcessCommand(commandText: string) : Result<Game, string> =
        let parts = commandText.Trim().Split([|' '|], 2, System.StringSplitOptions.RemoveEmptyEntries)
        let commandName = if parts.Length > 0 then parts[0] else ""
        let commandArgs = if parts.Length > 1 then parts[1] else ""

        match this.strategies.TryGetValue(commandName) with
        | true, strategy -> this.Execute(strategy) commandArgs
        | false, _ -> 
            match this.defaultStrategy with
            | Some strategy -> this.Execute(strategy) commandText // Pass full command to default
            | None -> Result.Error("Unknown command or no default handler, type \"help\" for available commands.") 