module fchess.CLI.Commands.MakeMoveCommand

open fchess.CLI.Commands.ICommandStrategy
open fchess.Core.Game
open fchess.Core.Move
open fchess.Core.Utils
open System.Text.RegularExpressions

let private toPly (pattern: string) =
    let moveRegex = @"^([a-h][1-8]){2}$"
    let queenside = "O-O-O"
    let kingside = "O-O"
    match pattern with
    | pattern when Regex.IsMatch(pattern, moveRegex) ->
        let startSquare = cellToIndex(pattern.Substring(0,2))
        let endSquare = cellToIndex(pattern.Substring(2,2))
        let move = {StartSquare = startSquare; EndSquare = endSquare; MoveFlag = MoveFlag.None}
        Some(Move(move))
    | pattern when pattern = queenside ->
        Some(Castle(Castle.Queenside))
    | pattern when pattern = kingside ->
        Some(Castle(Castle.Kingside))
    | _ -> None

let private makePly(state : Game, ply: Ply) =
    state.Board.MakePly(ply)
    Result.Ok state

type MakeMoveCommand() =
    interface ICommandStrategy with
        member this.Execute(state, moveString) =
            match toPly(moveString) with
                | Some(Move move) ->
                    let moves = state.GenerateMoves()
                    let move' = moves |> List.tryFind (
                        fun x -> x.EndSquare = move.EndSquare && x.StartSquare = move.StartSquare
                    )
                    match move' with
                    | Some foundMove -> makePly(state, Move(foundMove))
                    | None -> Result.Error("Move is not legal")
                | Some(Castle castle) ->
                    let castles = state.GenerateCastles()
                    let castle' = castles |> List.tryFind (fun x -> x = castle)
                    
                    match castle' with
                    | Some foundCastle -> makePly(state, Castle(foundCastle))
                    | None -> Result.Error("Cannot castle")
                | None -> Result.Error("Invalid move") 