module fchess.CLI.Commands.PositionCommand

open fchess.CLI.Commands.ICommandStrategy
open fchess.Core.Game
open fchess.Core.Utils
open System.Text.RegularExpressions

let private startPosFEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

let private (|FEN|NotFEN|) (pattern: string) =
    let fenRegex = @"^([rnbqkpRNBQKP1-8]+\/){7}[rnbqkpRNBQKP1-8]+ [wb] (K?Q?k?q?|-){1} ([a-h][36]|-) \d+ \d+$"
    if Regex.IsMatch(pattern, fenRegex) then FEN else NotFEN

type PositionCommand() =
    interface ICommandStrategy with
        member this.Execute(state, fen) =
            match fen with
            | "startpos" -> Result.Ok { state with Board = parseFEN startPosFEN}
            | FEN -> Result.Ok { state with Board = parseFEN fen}
            | NotFEN -> Result.Error("Invalid FEN")
