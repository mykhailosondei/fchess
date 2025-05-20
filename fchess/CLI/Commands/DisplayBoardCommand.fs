module fchess.CLI.Commands.DisplayBoardCommand

open fchess.CLI.Commands.ICommandStrategy
open fchess.Core.Game

type DisplayBoardCommand() =
    interface ICommandStrategy with
        member this.Execute state _args =
            printfn $"{state.Board.ToString()}"
            Result.Ok state 