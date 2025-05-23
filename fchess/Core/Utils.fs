module fchess.Core.Utils

open System
open Piece
open Board

let cellToIndex (cell: string) =
    let file = int cell[0] - int 'a'       // 'a' = 0, ..., 'h' = 7
    let rank = int cell[1] - int '1'       // '1' = 0, ..., '8' = 7
    rank * 8 + file
    
let indexToCell (index: int) =
    let file = index % 8
    let rank = index / 8
    $"{char (int 'a' + file)}{char (int '1' + rank)}"

let parseFEN (fen:string) = 
    let board = Board.init()
    let parts = fen.Split ' '
    let mutable rank = 7
    let mutable file = 0
    
    for c in parts[0].ToCharArray() do
        match c with
        | c when Char.IsLetter(c) ->
            board.Square[rank * 8 + file] <- charToPiece c
            file <- file + 1
        | c when Char.IsDigit(c) ->
            file <- file + Int32.Parse($"{c}")
        | '/' ->
            file <- 0
            rank <- rank - 1
        | _ -> ()
    
    match parts[1] with
    | "w" -> board.ColorToMove <- Piece.White
    | "b" -> board.ColorToMove <- Piece.Black
    | _ -> ()
    
    for c in parts[2] do
        board.CastleRights <- charToPiece c :: board.CastleRights
    
    match parts[3] with
    | "-" -> board.EnPassantSquare <- -1
    | square -> board.EnPassantSquare <- cellToIndex square
    
    board.Halfmoves <- Int32.Parse($"{parts[4]}")
    board.MoveNumber <- Int32.Parse($"{parts[5]}")
    
    board
        

let boardToFEN (board: Board) =
    let sb = System.Text.StringBuilder()
    
    // Piece placement
    for rank in 7 .. -1 .. 0 do
        let mutable empty = 0
        for file in 0 .. 7 do
            let idx = rank * 8 + file
            let piece = board.Square[idx]
            if piece = Piece.None then
                empty <- empty + 1
            else
                if empty > 0 then
                    sb.Append(string empty) |> ignore
                    empty <- 0
                sb.Append(pieceToChar piece) |> ignore
        if empty > 0 then
            sb.Append(string empty) |> ignore
        if rank > 0 then
            sb.Append('/') |> ignore

    // Color to move
    let colorStr = match board.ColorToMove with
                   | Piece.White -> " w"
                   | Piece.Black -> " b"
                   | _ -> " ?"
    sb.Append(colorStr) |> ignore

    // Castling rights
    let castlingStr =
        if board.CastleRights.IsEmpty then " -"
        else " " + (board.CastleRights |> List.map pieceToChar |> List.toArray |> String)
    sb.Append(castlingStr) |> ignore

    // En passant square
    let epStr =
        if board.EnPassantSquare = -1 then " -"
        else " " + indexToCell board.EnPassantSquare
    sb.Append(epStr) |> ignore

    // Halfmove clock and fullmove number
    sb.AppendFormat(" {0} {1}", board.Halfmoves, board.MoveNumber) |> ignore

    sb.ToString()

    

type LogLevel =
    | Info
    | Debug
    | Warning
    | Error

type LogEntry<'T> = {
    Timestamp: DateTime
    Level: LogLevel
    Message: string
    Data: 'T
}

let createLogEntry<'T> (level: LogLevel) (message: string) (data: 'T) : LogEntry<'T> =
    {
        Timestamp = DateTime.UtcNow
        Level = level
        Message = message
        Data = data
    }

let log<'T> (entry: LogEntry<'T>) : unit =
    let logString =
        sprintf "[%s] [%A] %s - Data: %A"
            (entry.Timestamp.ToString("yyyy-MM-dd HH:mm:ss.fff"))
            entry.Level
            entry.Message
            entry.Data
    printfn "%s" logString

    

