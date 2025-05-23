module fchess.Core.Game

open Board
open Piece
open fchess.Core.Move
open fchess.Core.Utils

type Game = { mutable Board : Board; } with
    member this.GenerateLegalMoves() =
        
        let originalBoard = this.Board.CopyBoard()
        
        let allMoves = this.Board.GenerateMoves()
        
        let result =
            allMoves |> List.filter (fun move ->
                this.Board.MakeMove(move)
                
                let opponentMoves = this.Board.GenerateMoves()
                
                let isKingCapture = opponentMoves |> List.exists (fun m ->
                    getPieceType this.Board.Square[m.EndSquare] = Piece.King) |> not
                
                this.Board <- originalBoard.CopyBoard()
                
                isKingCapture
            )
            
        this.Board <- originalBoard.CopyBoard()
        
        result
        
    member this.GeneratePerft(depth : int) =
        let mutable strings = []
        let rec perft d : int =
            let originalBoard = this.Board.CopyBoard()
            
            if d = 0
                then
                    1
            else
                let moves = this.GenerateLegalMoves()
                let castles = this.Board.GenerateCastles()
                let mutable sum = 0
                for move in moves do
                    this.Board.MakeMove(move)
                    let search = perft (d-1)
                    sum <- sum + search
                    if d = depth then strings <- $"{indexToCell move.StartSquare}{indexToCell move.EndSquare}: {search}" :: strings
                    this.Board <- originalBoard.CopyBoard()
                for castle in castles do
                    this.Board.MakeCastle(castle)
                    let search = perft (d-1)
                    sum <- sum + search
                    if d = depth then strings <- $"{castle}: {search}" :: strings
                    this.Board <- originalBoard.CopyBoard()
                sum
        
        
        let result = perft depth
        
        strings |> List.sort |> List.iter (fun s ->
            printfn $"{s}"
            )
        
        result
        
    member this.Search(depth : int) =
        let rec search d =
            let originalBoard = this.Board.CopyBoard()
            
            if d = 0 then
                this.Board.Evaluate()
            else
                let moves = this.GenerateLegalMoves()
                let castles = this.Board.GenerateCastles()
                if moves.Length + castles.Length = 0 then
                    if this.Board.IsInCheck() then
                        -1000000
                    else 0
                else
                    let mutable bestEval = -1000000
                    for move in moves do
                        this.Board.MakeMove(move)
                        let eval = -search (d-1)
                        bestEval <- max eval bestEval
                        this.Board <- originalBoard.CopyBoard()
                        
                    for castle in castles do
                        this.Board.MakeCastle(castle)
                        let eval = -search (d-1)
                        bestEval <- max eval bestEval
                        this.Board <- originalBoard.CopyBoard()
                    bestEval
                
        search depth
            
    member this.ChooseBestPly() =
        let moves = this.GenerateLegalMoves()
        let castles = this.Board.GenerateCastles()
        let originalBoard = this.Board.CopyBoard()
        
        let mutable bestEval = -1000000
        let mutable bestMove : Ply option = None 
        
        for move in moves do
            this.Board.MakeMove(move)
            let eval = -this.Search(2)
            if eval > bestEval then
                bestMove <- Some(Move(move))
                bestEval <- eval
            this.Board <- originalBoard.CopyBoard()
        
        for castle in castles do
            this.Board.MakeCastle(castle)
            let eval = -this.Search(2)
            if eval > bestEval then
                bestMove <- Some(Castle(castle))
                bestEval <- eval
            this.Board <- originalBoard.CopyBoard()
        
        bestMove
                
            

let init() =
    { Board = init(); }
    
let initWithBoard board =
    { Board = board }