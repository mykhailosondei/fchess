module fchess.Core.Game

open Board
open fchess.Core.Move
open fchess.Core.Piece

type Game = { Board : Board; mutable moves : Move list} with
    member this.GenerateMoves() =
        this.moves <- []
        
        for square = 0 to 63 do
            let piece = this.Board.Square[square]
            
            match piece with
            | piece when isColor piece this.Board.ColorToMove ->
                match piece with
                | piece when isSliding piece ->
                    this.GenerateSlidingMoves(square, piece)
                | piece when getPieceType piece = Piece.Knight ->
                    this.GenerateKnightMoves(square)
                | piece when getPieceType piece = Piece.Pawn ->
                    this.GeneratePawnMoves(square)
                | _ -> ()
            | _ -> ()
            
        this.moves
        
    member private this.DetermineFlag(startSquare, endSquare, pieceType) =
        let mutable result = MoveFlag.None
        if isColor this.Board.Square[endSquare] (oppositeColor this.Board.ColorToMove) then
            result <- result ||| MoveFlag.Capture
        
        match pieceType with
        | Piece.Rook ->
            match this.Board.ColorToMove with
            | Piece.White ->
                match startSquare with
                | 0 -> result <- result ||| MoveFlag.QueenRookMove
                | 7 -> result <- result ||| MoveFlag.KingRookMove
                | _ -> ()
            | Piece.Black ->
                match startSquare with
                | 56 -> result <- result ||| MoveFlag.QueenRookMove
                | 63 -> result <- result ||| MoveFlag.KingRookMove
                | _ -> ()
            | _ -> ()
        | Piece.Knight -> ()
        | Piece.Pawn -> result <- result ||| MoveFlag.PawnMove
        | _ -> ()
        
        result
            
    member this.GenerateSlidingMoves(startSquare, piece) =
        let pieceType = getPieceType piece
        let startDirection = if Piece.Bishop = pieceType then 3 else 0
        let endDirection = if Piece.Rook = pieceType then 3 else 7
        for direction = startDirection to endDirection do
            let offset = directionOffsets[direction]
            let maxSteps = numSquaresToEdge[startSquare][direction]

            let rec slide n =
                if n >= maxSteps then () else
                let endSquare = startSquare + offset * (n + 1)
                let targetPiece = this.Board.Square[endSquare]

                if isColor targetPiece this.Board.ColorToMove then
                    () // Friendly piece blocks the path; stop
                else
                // Add the move
                
                let flag = this.DetermineFlag(startSquare, endSquare, pieceType)
                let move = { StartSquare = startSquare; EndSquare = endSquare; MoveFlag = flag}
                
                this.moves <- move :: this.moves

                if not (isEmpty targetPiece) then
                    () // Enemy piece captured; stop sliding
                else
                    slide (n + 1) // Keep sliding
                
            slide 0
            
    member this.GenerateKnightMoves(startSquare) =
        let (file, rank) = indexToCoord startSquare
        knightMoves
        |> List.iter (fun (dx, dy) ->
            let file', rank' = file + dx, rank + dy
            let endSquare = coordToIndex (file', rank')
            if isOnBoard file' rank' && not <| isColor this.Board.Square[endSquare] this.Board.ColorToMove then
                let flag = this.DetermineFlag(startSquare, endSquare, Piece.Knight)
                let move = { StartSquare = startSquare; EndSquare = endSquare; MoveFlag = flag}
                
                this.moves <- move :: this.moves
        )
        
    member this.GeneratePawnMoves(startSquare) =
        let mutable homeRank = 0
        let mutable pawnDirection = 0
        let mutable beforePromotionRank = 0
        match this.Board.ColorToMove with
        | Piece.White ->
            homeRank <- 1
            pawnDirection <- 8
            beforePromotionRank <- 6
        | Piece.Black ->
            homeRank <- 6
            pawnDirection <- -8
            beforePromotionRank <- 1
        | _ -> ()
        
        let startRank = startSquare / 8
        
        let addNormalMoves promotion =
            let endSquare = startSquare + pawnDirection
            let promotions = if promotion then [|MoveFlag.PromoteToBishop; MoveFlag.PromoteToKnight; MoveFlag.PromoteToQueen; MoveFlag.PromoteToRook|] else [|MoveFlag.None|]
                
            promotions |> Array.iter (fun flag ->
                let move = { StartSquare = startSquare; EndSquare = endSquare; MoveFlag = flag ||| MoveFlag.PawnMove}
                this.moves <- move :: this.moves)
            
        let addDoubleMoves =
            let endSquare = startSquare + 2 * pawnDirection
            let flag = MoveFlag.PawnMove ||| MoveFlag.DoublePawnMove
            let moveDouble = { StartSquare = startSquare; EndSquare = endSquare; MoveFlag = flag}
                
            this.moves <- moveDouble :: this.moves
            
        let addCaptures promotion =
            let mutable endSquares = []
            
            if not (startSquare % 8 = 0) then endSquares <- startSquare + pawnDirection - 1 :: endSquares
            if not (startSquare % 8 = 7) then endSquares <- startSquare + pawnDirection + 1 :: endSquares
            
            endSquares |> List.iter (fun endSquare ->
                if isColor this.Board.Square[endSquare] (oppositeColor this.Board.ColorToMove) then
                    let promotions = if promotion then [|MoveFlag.PromoteToBishop; MoveFlag.PromoteToKnight; MoveFlag.PromoteToQueen; MoveFlag.PromoteToRook|] else [|MoveFlag.None|]
                
                    promotions |> Array.iter (fun flag ->
                    let move = { StartSquare = startSquare; EndSquare = endSquare; MoveFlag = flag ||| MoveFlag.PawnMove ||| MoveFlag.Capture}
                    this.moves <- move :: this.moves)
                )
            
        
        match startRank with
        | startRank when startRank = homeRank ->
            if isEmpty this.Board.Square[startSquare + pawnDirection] then
                addNormalMoves false
                if isEmpty this.Board.Square[startSquare + 2 * pawnDirection] then
                    addDoubleMoves
                    
            addCaptures false
            
        
        | startRank when startRank = beforePromotionRank ->
            if isEmpty this.Board.Square[startSquare + pawnDirection] then
                addNormalMoves true
            
            addCaptures true
            
        | _ ->
            if isEmpty this.Board.Square[startSquare + pawnDirection] then
                addNormalMoves false
            
            addCaptures false
            
        if not(this.Board.EnPassantSquare = -1) then
            if startSquare + pawnDirection + 1 = this.Board.EnPassantSquare || startSquare + pawnDirection - 1 = this.Board.EnPassantSquare then
                let flag = MoveFlag.PawnMove ||| MoveFlag.Capture ||| MoveFlag.EnPassant
                let move = { StartSquare = startSquare; EndSquare = this.Board.EnPassantSquare; MoveFlag = flag}
                this.moves <- move :: this.moves            
                
            
        
        
        
                
                    
                
                
                
            

let init() =
    { Board = init(); moves = [] }
    
    