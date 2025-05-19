module fchess.Core.Piece

open System

[<Flags>]
type Piece =
    | None   = 0
    
    // Piece types (lower 3 bits)
    | King    = 1
    | Pawn    = 2
    | Knight  = 3
    | Bishop  = 4
    | Rook    = 5
    | Queen   = 6
    
    // Colors (higher bits)
    | White   = 8
    | Black   = 16
    
let pieceTypeMask = enum<Piece> 0b00111
let colorMask     = enum<Piece> 0b11000

let colorOfCase c =
    match c with
    | c when Char.IsLower(c) -> Piece.Black
    | _ -> Piece.White
    

let charToPiece c =
    match c with
    | 'r'
    | 'R' -> Piece.Rook ||| colorOfCase c
    | 'n'
    | 'N' -> Piece.Knight ||| colorOfCase c 
    | 'k'
    | 'K' -> Piece.King ||| colorOfCase c
    | 'b'
    | 'B' -> Piece.Bishop ||| colorOfCase c
    | 'q'
    | 'Q' -> Piece.Queen ||| colorOfCase c
    | 'p'
    | 'P' -> Piece.Pawn ||| colorOfCase c
    | _ -> Piece.None

let getColor (p: Piece) =
    match p &&& colorMask with
    | Piece.White -> Some Piece.White
    | Piece.Black -> Some Piece.Black
    | _ -> None
    
let oppositeColor (color: Piece) =
    match color with
    | Piece.White -> Piece.Black
    | Piece.Black -> Piece.White
    | _ -> Piece.None
    
let isColor (p: Piece)  (color : Piece) =
    p &&& colorMask = color
    
let sameColor (p1: Piece, p2: Piece) =
    getColor(p1) = getColor(p2)

let getPieceType (p: Piece) =
    p &&& pieceTypeMask

let isEmpty (p: Piece) =
    p = Piece.None
    
let isSliding piece =
    match getPieceType(piece) with
    | Piece.Rook
    | Piece.Queen
    | Piece.Bishop -> true
    | _ -> false
    
    
let pieceToChar (p: Piece) : char =
    let isWhite = isColor p Piece.White
    let pieceType = getPieceType p

    match pieceType with
    | Piece.Rook -> if isWhite then 'R' else 'r'
    | Piece.Knight -> if isWhite then 'N' else 'n'
    | Piece.Bishop -> if isWhite then 'B' else 'b'
    | Piece.Queen -> if isWhite then 'Q' else 'q'
    | Piece.King -> if isWhite then 'K' else 'k'
    | Piece.Pawn -> if isWhite then 'P' else 'p'
    | _ -> ' '