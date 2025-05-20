module fchess.CLI.CommandProcessor

open System.Collections.Generic
open fchess.CLI.Commands.ICommandStrategy
open fchess.CLI.Commands.PositionCommand
open fchess.CLI.Commands.DisplayBoardCommand
open fchess.CLI.Commands.DisplayMovesCommand
open fchess.CLI.Commands.MakeMoveCommand
open fchess.Core.Game

type CommandProcessor =
    val strategies: Dictionary<string, ICommandStrategy>
    val mutable defaultStrategy: ICommandStrategy option

    new() =
        let strategies = Dictionary<string, ICommandStrategy>()
        strategies.Add("position", PositionCommand() :> ICommandStrategy)
        strategies.Add("d", DisplayBoardCommand() :> ICommandStrategy)
        strategies.Add("moves", DisplayMovesCommand() :> ICommandStrategy)
        strategies.Add("move", MakeMoveCommand() :> ICommandStrategy)
        { strategies = strategies; defaultStrategy = None }

    new(customStrategies: seq<string * ICommandStrategy>) =
        let strategies = Dictionary<string, ICommandStrategy>()
        strategies.Add("position", PositionCommand() :> ICommandStrategy)
        strategies.Add("d", DisplayBoardCommand() :> ICommandStrategy)
        strategies.Add("moves", DisplayMovesCommand() :> ICommandStrategy)
        strategies.Add("move", MakeMoveCommand() :> ICommandStrategy)
        for (cmd, strategy) in customStrategies do
            strategies.[cmd] <- strategy
        { strategies = strategies; defaultStrategy = None }
        
    member this.DefaultStrategy
        with get() = this.defaultStrategy
        and set(strategy: ICommandStrategy option) = this.defaultStrategy <- strategy

    member this.RegisterStrategy(commandName: string, strategy: ICommandStrategy) =
        this.strategies.[commandName] <- strategy

    member this.ProcessCommand(state: Game, commandText: string) : Result<Game, string> =
        let parts = commandText.Trim().Split([|' '|], 2, System.StringSplitOptions.RemoveEmptyEntries)
        let commandName = if parts.Length > 0 then parts.[0] else ""
        let commandArgs = if parts.Length > 1 then parts.[1] else ""

        match this.strategies.TryGetValue(commandName) with
        | true, strategy -> strategy.Execute(state, commandArgs)
        | false, _ -> 
            match this.defaultStrategy with
            | Some strategy -> strategy.Execute(state, commandText) // Pass full command to default
            | None -> Result.Error("Unknown command or no default handler, type \"help\" for available commands.") 