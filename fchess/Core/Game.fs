module fchess.Core.Game

open Board
open fchess.Core.Move
open fchess.Core.Piece



type Game = { Board : Board; mutable moves : Move list; mutable castles : Castle list} with
    member this.GenerateMoves() =
        let mutable moves : Move list = []
        
        for square = 0 to 63 do
            let piece = this.Board.Square[square]
            
            match piece with
            | piece when isColor piece this.Board.ColorToMove ->
                match piece with
                | piece when isSliding piece ->
                    moves <- this.GenerateSlidingMoves(square, piece) |> List.append moves
                | piece when getPieceType piece = Piece.Knight ->
                    moves <- this.GenerateKnightMoves(square) |> List.append moves
                | piece when getPieceType piece = Piece.Pawn ->
                    moves <- this.GeneratePawnMoves(square) |> List.append moves
                | piece when getPieceType piece = Piece.King ->
                    moves <- this.GenerateKingMoves(square) |> List.append moves
                | _ -> ()
            | _ -> ()
            
        moves
        
    member this.DetermineFlag(startSquare, endSquare, pieceType) =
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
        | Piece.King -> result <- result ||| MoveFlag.KingMove
        | Piece.Pawn -> result <- result ||| MoveFlag.PawnMove
        | _ -> ()
        
        result
            
    member this.GenerateSlidingMoves(startSquare, piece) =
        let mutable moves : Move list = []
        let pieceType = getPieceType piece
        let startDirection = if Piece.Bishop = pieceType then 4 else 0
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
                
                moves <- move :: moves

                if not (isEmpty targetPiece) then
                    () // Enemy piece captured; stop sliding
                else
                    slide (n + 1) // Keep sliding
                
            slide 0
            
        moves
        
    member this.GenerateKnightMoves(startSquare) =
        let mutable moves : Move list = []
        let file, rank = indexToCoord startSquare
        knightMoves
        |> List.iter (fun (dx, dy) ->
            let file', rank' = file + dx, rank + dy
            let endSquare = coordToIndex (file', rank')
            if isOnBoard file' rank' && not <| isColor this.Board.Square[endSquare] this.Board.ColorToMove then
                let flag = this.DetermineFlag(startSquare, endSquare, Piece.Knight)
                let move = { StartSquare = startSquare; EndSquare = endSquare; MoveFlag = flag}
                
                moves <- move :: moves
        )
        
        moves
        
    member this.GeneratePawnMoves(startSquare) =
        let mutable moves : Move list = []
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
                moves <- move :: moves)
            
        let addDoubleMoves() =
            let endSquare = startSquare + 2 * pawnDirection
            let flag = MoveFlag.PawnMove ||| MoveFlag.DoublePawnMove
            let moveDouble = { StartSquare = startSquare; EndSquare = endSquare; MoveFlag = flag}
                
            moves <- moveDouble :: moves
            ()
            
        let addCaptures promotion =
            let mutable endSquares = []
            
            if not (startSquare % 8 = 0) then endSquares <- startSquare + pawnDirection - 1 :: endSquares
            if not (startSquare % 8 = 7) then endSquares <- startSquare + pawnDirection + 1 :: endSquares
            
            endSquares |> List.iter (fun endSquare ->
                if isColor this.Board.Square[endSquare] (oppositeColor this.Board.ColorToMove) then
                    let promotions = if promotion then [|MoveFlag.PromoteToBishop; MoveFlag.PromoteToKnight; MoveFlag.PromoteToQueen; MoveFlag.PromoteToRook|] else [|MoveFlag.None|]
                
                    promotions |> Array.iter (fun flag ->
                    let move = { StartSquare = startSquare; EndSquare = endSquare; MoveFlag = flag ||| MoveFlag.PawnMove ||| MoveFlag.Capture}
                    moves <- move :: moves)
                )
            
        
        match startRank with
        | rank when rank = homeRank ->
            if isEmpty this.Board.Square[startSquare + pawnDirection] then
                addNormalMoves false
                if isEmpty this.Board.Square[startSquare + 2 * pawnDirection] then
                    addDoubleMoves ()
                    
            addCaptures false
            
        | rank when rank = beforePromotionRank ->
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
                moves <- move :: moves
                
        moves
                
    member this.GenerateKingMoves(startSquare) =
        let mutable moves : Move list = []
        let file, rank = indexToCoord startSquare
        kingMoves
            |> List.iter (fun (dx, dy) ->
            let file', rank' = file + dx, rank + dy
            let endSquare = coordToIndex (file', rank')
            if isOnBoard file' rank' && not <| isColor this.Board.Square[endSquare] this.Board.ColorToMove then
                let flag = this.DetermineFlag(startSquare, endSquare, Piece.King)
                let move = { StartSquare = startSquare; EndSquare = endSquare; MoveFlag = flag}
            
                moves <- move :: moves
        )
        
        moves
            
    
    member private this.checkAttacked(square) =
        this.Board.ColorToMove <- oppositeColor this.Board.ColorToMove
        
        let moves = this.GenerateMoves()
        
        this.Board.ColorToMove <- oppositeColor this.Board.ColorToMove
        
        moves |> List.exists (fun move -> move.EndSquare = square)
        
        
    member this.GenerateCastles() = 
        let startSquare =
            match this.Board.ColorToMove with
            | Piece.White -> 4
            | Piece.Black -> 60
        
        
        let mutable moves : Castle list = []
        
        if this.Board.CastleRights |> List.contains (Piece.King ||| this.Board.ColorToMove) then
            let rookSquare = startSquare + 3
            
            if getPieceType this.Board.Square[rookSquare] = Piece.Rook then
                let isCastlable = [startSquare + 1; startSquare + 2] |> List.forall ( fun square ->
                    let isAttacked = this.checkAttacked(square)
                    let isClear = getPieceType this.Board.Square[square] = Piece.None
                    
                    isClear && not isAttacked
                    )
                
                if isCastlable then
                    moves <- Castle.Kingside :: moves
            
        if this.Board.CastleRights |> List.contains (Piece.Queen ||| this.Board.ColorToMove) then
            let rookSquare = startSquare - 4
            
            if getPieceType this.Board.Square[rookSquare] = Piece.Rook then
                let isClear = [startSquare - 1; startSquare - 2; startSquare - 3;] |> List.forall ( fun square ->
                    let isClear = getPieceType this.Board.Square[square] = Piece.None
                    
                    isClear
                    )
                
                let isNotAttackable = [startSquare; startSquare - 1; startSquare - 2] |> List.forall ( fun square ->
                    let isAttacked = this.checkAttacked(square)
                    
                    not isAttacked
                    )
                
                if isClear && isNotAttackable then
                    moves <- Castle.Queenside :: moves
                    
        if not (getPieceType this.Board.Square[startSquare] = Piece.King) then
            moves <- []
            
        moves

    
let init() =
    { Board = init(); moves = []; castles = [] }    