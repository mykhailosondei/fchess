module fchess.Core.Move

open System

[<Flags>]
type MoveFlag =
    | None = 0
    | QueenRookMove = 1
    | KingRookMove = 2
    | DoublePawnMove = 3
    | PromoteToQueen = 4
    | PromoteToKnight = 5
    | PromoteToRook = 6
    | PromoteToBishop = 7
    | FirstKingMove = 8
    | EnPassant = 9
    | Capture = 16
    | PawnMove = 32
    
let specialtyMask = enum<MoveFlag> 0b001111
let ruleBreakMask     = enum<MoveFlag> 0b110000

let specialty flag =
    flag &&& specialtyMask
    
let breaksRule flag =
    flag &&& ruleBreakMask = MoveFlag.None |> not
    
let isCapture flag =
    flag &&& MoveFlag.Capture = MoveFlag.Capture
    
    
type Move = { StartSquare : int; EndSquare : int; MoveFlag : MoveFlag }

type Castle =
    | Queenside
    | Kingside

type Ply =
    | Move of Move
    | Castle of Castle

let numSquaresToEdge : int array array =
    let result = Array.create 64 [||]
    for file = 0 to 7 do
        for rank = 0 to 7 do
            let numNorth = 7 - rank
            let numSouth = rank
            let numWest = file
            let numEast = 7 - file
            
            result[rank * 8 + file] <- [|
                numNorth
                numSouth
                numWest
                numEast
                min numNorth numWest
                min numSouth numEast
                min numNorth numEast
                min numSouth numWest
            |]
    result
    
let directionOffsets = [|
                         8
                         -8
                         -1
                         1
                         7
                         -7
                         9
                         -9
                         |]

let isOnBoard (x: int) (y: int) =
    x >= 0 && x < 8 && y >= 0 && y < 8

let indexToCoord (i: int) =
    let x = i % 8
    let y = i / 8
    (x, y)

let coordToIndex (x: int, y: int) =
    y * 8 + x

let knightMoves =
    [ (-2, -1); (-1, -2); (1, -2); (2, -1)
      (-2, 1);  (-1, 2);  (1, 2);  (2, 1) ]