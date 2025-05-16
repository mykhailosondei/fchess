module fchess.Core.Utils

open System
open Piece

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
        
       
    

