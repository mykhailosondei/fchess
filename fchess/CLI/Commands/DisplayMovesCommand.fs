module fchess.CLI.Commands.DisplayMovesCommand

open fchess.CLI.Commands.ICommandStrategy
open fchess.Core.Utils

type DisplayMovesCommand() =
    interface ICommandStrategy with
        member this.Execute state _args =
            let moves = state.GenerateMoves()
            moves |> List.iter (fun move ->
                printfn $"{indexToCell move.StartSquare} {indexToCell move.EndSquare} {move.MoveFlag}"
                )
            
            let castles = state.GenerateCastles()
            
            castles |> List.iter (fun castle ->
                let message = "O-O".Insert(0, if castle.IsKingside then "" else "O-")
                printfn $"%s{message}"
                )
            
            Result.Ok state 