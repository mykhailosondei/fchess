module fchess.Core.Board

open Piece
open Move

type Board = {
    Square: Piece array
    mutable ColorToMove: Piece
    mutable CastleRights: Piece list
    mutable EnPassantSquare: int
    mutable Halfmoves: int
    mutable MoveNumber: int
}
with
    override this.ToString() =
        let sb = System.Text.StringBuilder()
        
        for rank in 7 .. -1 .. 0 do
            sb.Append(sprintf "%d " (rank + 1)) |> ignore
            for file in 0 .. 7 do
                let index = rank * 8 + file
                let piece = this.Square[index]
                let symbol =
                    match piece &&& enum<Piece> 0b00111, piece &&& enum<Piece> 0b11000 with
                    | Piece.None, _ -> '.'
                    | p, Piece.White ->
                        match p with
                        | Piece.Pawn -> 'P'
                        | Piece.Knight -> 'N'
                        | Piece.Bishop -> 'B'
                        | Piece.Rook -> 'R'
                        | Piece.Queen -> 'Q'
                        | Piece.King -> 'K'
                        | _ -> '?'
                    | p, Piece.Black ->
                        match p with
                        | Piece.Pawn -> 'p'
                        | Piece.Knight -> 'n'
                        | Piece.Bishop -> 'b'
                        | Piece.Rook -> 'r'
                        | Piece.Queen -> 'q'
                        | Piece.King -> 'k'
                        | _ -> '?'
                    | _ -> '?'
                sb.Append(symbol).Append(' ') |> ignore
            sb.AppendLine() |> ignore
        
        sb.AppendLine("  a b c d e f g h") |> ignore
        sb.AppendLine(sprintf "Turn: %s" (if this.ColorToMove = Piece.White then "White" else "Black")) |> ignore
        sb.AppendLine(sprintf "Castling: %s" (
            if this.CastleRights.IsEmpty then "-" 
            else 
                this.CastleRights 
                |> List.map (fun p -> 
                    match p with
                    | p when p = (Piece.White ||| Piece.King) -> "K"
                    | p when p = (Piece.White ||| Piece.Queen) -> "Q"
                    | p when p = (Piece.Black ||| Piece.King) -> "k"
                    | p when p = (Piece.Black ||| Piece.Queen) -> "q"
                    | _ -> "") 
                |> String.concat "")
        ) |> ignore
        sb.AppendLine(sprintf "En Passant: %s" (
            if this.EnPassantSquare = -1 then "-" 
            else 
                let file = this.EnPassantSquare % 8
                let rank = this.EnPassantSquare / 8
                sprintf "%c%d" (char (file + int 'a')) (rank + 1)
        )) |> ignore
        sb.AppendLine(sprintf "Halfmoves: %d" this.Halfmoves) |> ignore
        sb.AppendLine(sprintf "Move Number: %d" this.MoveNumber) |> ignore
        
        sb.ToString()
    
    member this.MakePly(ply : Ply) =
        match ply with
        | Move move -> this.MakeMove(move)
        | Castle castle -> this.MakeCastle(castle)

    member this.MakeCastle(castle) =
        let mutable kingStart = 0
        let mutable rookStart = 0
        let mutable kingFinish = 0
        let mutable rookFinish = 0
        match castle with
        | Kingside ->
            if this.ColorToMove = Piece.White then
                kingStart <- 4
                rookStart <- 7
                kingFinish <- 6
                rookFinish <- 5
            else
                kingStart <- 60
                rookStart <- 63
                kingFinish <- 62
                rookFinish <- 61
        | Queenside ->
            if this.ColorToMove = Piece.White then
                kingStart <- 4
                rookStart <- 0
                kingFinish <- 2
                rookFinish <- 3
            else
                kingStart <- 60
                rookStart <- 56
                kingFinish <- 58
                rookFinish <- 59
                
        this.Square[kingFinish] <- this.Square[kingStart]
        this.Square[kingStart] <- Piece.None
                
        this.Square[rookFinish] <- this.Square[rookStart]
        this.Square[rookStart] <- Piece.None
        
        this.CastleRights <- this.CastleRights |> List.where (fun x -> (isColor x <| oppositeColor this.ColorToMove))
        this.EnPassantSquare <- -1
        this.Halfmoves <- this.Halfmoves + 1
        
        if this.ColorToMove = Piece.Black then
            this.MoveNumber <- this.MoveNumber + 1
        
        this.ColorToMove <- oppositeColor this.ColorToMove
    
    
    member this.MakeMove(move : Move) =
        
        let mutable endSquarePiece = Piece.None
        
        match specialty move.MoveFlag with
        | MoveFlag.PromoteToRook ->
            assert (getPieceType this.Square[move.StartSquare] = Piece.Pawn)
            endSquarePiece <- Piece.Rook ||| this.ColorToMove
        | MoveFlag.PromoteToQueen ->
            assert (getPieceType this.Square[move.StartSquare] = Piece.Pawn)
            endSquarePiece <- Piece.Queen ||| this.ColorToMove
        | MoveFlag.PromoteToKnight ->
            assert (getPieceType this.Square[move.StartSquare] = Piece.Pawn)
            endSquarePiece <- Piece.Knight ||| this.ColorToMove
        | MoveFlag.PromoteToBishop ->
            assert (getPieceType this.Square[move.StartSquare] = Piece.Pawn)
            endSquarePiece <- Piece.Bishop ||| this.ColorToMove
        | _ ->
            endSquarePiece <- this.Square[move.StartSquare]
          
        let colorFilter = isColor <| oppositeColor this.ColorToMove
            
        match specialty move.MoveFlag with
        | MoveFlag.KingMove ->
            assert (getPieceType this.Square[move.StartSquare] = Piece.King)
            this.CastleRights <- this.CastleRights |> List.where colorFilter
        | MoveFlag.QueenRookMove ->
            assert (getPieceType this.Square[move.StartSquare] = Piece.Rook)
            this.CastleRights <- this.CastleRights |> List.where (fun right -> not (Piece.Queen ||| this.ColorToMove = right))
        | MoveFlag.KingRookMove ->
            assert (getPieceType this.Square[move.StartSquare] = Piece.Rook)
            this.CastleRights <- this.CastleRights |> List.where (fun right -> not (Piece.King |||  this.ColorToMove = right))
        | _ -> ()
        
        this.Halfmoves <- this.Halfmoves + 1
        
        if breaksRule move.MoveFlag then
            this.Halfmoves <- 0
        
        if this.ColorToMove = Piece.Black then
            this.MoveNumber <- this.MoveNumber + 1
        
        this.ColorToMove <- oppositeColor this.ColorToMove
        
        
        if specialty move.MoveFlag = MoveFlag.EnPassant then
            assert (move.EndSquare = this.EnPassantSquare)
            assert (getPieceType this.Square[move.StartSquare] = Piece.Pawn)
            let pawnShift = move.EndSquare % 8 - move.StartSquare % 8
            this.Square[move.StartSquare + pawnShift] <- Piece.None
        
        if specialty move.MoveFlag = MoveFlag.DoublePawnMove then
            assert (getPieceType this.Square[move.StartSquare] = Piece.Pawn)
            this.EnPassantSquare <- (move.StartSquare + move.EndSquare) / 2
        else this.EnPassantSquare <- -1
            
        this.Square[move.StartSquare] <- Piece.None
        this.Square[move.EndSquare] <- endSquarePiece
        
    member this.GenerateMoves() =
        let mutable moves : Move list = []
        
        for square = 0 to 63 do
            let piece = this.Square[square]
            
            match piece with
            | piece when isColor piece this.ColorToMove ->
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
        if isColor this.Square[endSquare] (oppositeColor this.ColorToMove) then
            result <- result ||| MoveFlag.Capture
        
        match pieceType with
        | Piece.Rook ->
            match this.ColorToMove with
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
                let targetPiece = this.Square[endSquare]

                if isColor targetPiece this.ColorToMove then
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
            if isOnBoard file' rank' && not <| isColor this.Square[endSquare] this.ColorToMove then
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
        match this.ColorToMove with
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
                if isColor this.Square[endSquare] (oppositeColor this.ColorToMove) then
                    let promotions = if promotion then [|MoveFlag.PromoteToBishop; MoveFlag.PromoteToKnight; MoveFlag.PromoteToQueen; MoveFlag.PromoteToRook|] else [|MoveFlag.None|]
                
                    promotions |> Array.iter (fun flag ->
                    let move = { StartSquare = startSquare; EndSquare = endSquare; MoveFlag = flag ||| MoveFlag.PawnMove ||| MoveFlag.Capture}
                    moves <- move :: moves)
                )
            
        
        match startRank with
        | rank when rank = homeRank ->
            if isEmpty this.Square[startSquare + pawnDirection] then
                addNormalMoves false
                if isEmpty this.Square[startSquare + 2 * pawnDirection] then
                    addDoubleMoves ()
                    
            addCaptures false
            
        | rank when rank = beforePromotionRank ->
            if isEmpty this.Square[startSquare + pawnDirection] then
                addNormalMoves true
            
            addCaptures true
            
        | _ ->
            if isEmpty this.Square[startSquare + pawnDirection] then
                addNormalMoves false
            
            addCaptures false
            
        let startFile = startSquare % 8
        
        if not(this.EnPassantSquare = -1) then
            if (startSquare + pawnDirection + 1 = this.EnPassantSquare && not (startFile = 7))
               ||
               (startSquare + pawnDirection - 1 = this.EnPassantSquare && not (startFile = 0))
                then
                let flag = MoveFlag.PawnMove ||| MoveFlag.Capture ||| MoveFlag.EnPassant
                let move = { StartSquare = startSquare; EndSquare = this.EnPassantSquare; MoveFlag = flag}
                moves <- move :: moves
                
        moves
                
    member this.GenerateKingMoves(startSquare) =
        let mutable moves : Move list = []
        let file, rank = indexToCoord startSquare
        kingMoves
            |> List.iter (fun (dx, dy) ->
            let file', rank' = file + dx, rank + dy
            let endSquare = coordToIndex (file', rank')
            if isOnBoard file' rank' && not <| isColor this.Square[endSquare] this.ColorToMove then
                let flag = this.DetermineFlag(startSquare, endSquare, Piece.King)
                let move = { StartSquare = startSquare; EndSquare = endSquare; MoveFlag = flag}
            
                moves <- move :: moves
        )
        
        moves
            
    
    member private this.checkAttacked(square) =
        this.ColorToMove <- oppositeColor this.ColorToMove
        
        let moves = this.GenerateMoves()
        
        this.ColorToMove <- oppositeColor this.ColorToMove
        
        moves |> List.exists (fun move -> move.EndSquare = square)
        
        
    member this.GenerateCastles() = 
        let startSquare =
            match this.ColorToMove with
            | Piece.White -> 4
            | Piece.Black -> 60
            | _ -> failwith "Board must be either black or white"
        
        
        let mutable moves : Castle list = []
        
        if this.CastleRights |> List.contains (Piece.King ||| this.ColorToMove) then
            let rookSquare = startSquare + 3
            
            if getPieceType this.Square[rookSquare] = Piece.Rook then
                let isClear = [startSquare + 1; startSquare + 2;] |> List.forall ( fun square ->
                    let isClear = getPieceType this.Square[square] = Piece.None
                    
                    isClear
                    )
                
                let isNotAttackable = [startSquare; startSquare + 1; startSquare + 2] |> List.forall ( fun square ->
                    let isAttacked = this.checkAttacked(square)
                    
                    not isAttacked
                    )
                
                if isClear && isNotAttackable then
                    moves <- Castle.Kingside :: moves
            
        if this.CastleRights |> List.contains (Piece.Queen ||| this.ColorToMove) then
            let rookSquare = startSquare - 4
            
            if getPieceType this.Square[rookSquare] = Piece.Rook then
                let isClear = [startSquare - 1; startSquare - 2; startSquare - 3;] |> List.forall ( fun square ->
                    let isClear = getPieceType this.Square[square] = Piece.None
                    
                    isClear
                    )
                
                let isNotAttackable = [startSquare; startSquare - 1; startSquare - 2] |> List.forall ( fun square ->
                    let isAttacked = this.checkAttacked(square)
                    
                    not isAttacked
                    )
                
                if isClear && isNotAttackable then
                    moves <- Castle.Queenside :: moves
                    
        if not (getPieceType this.Square[startSquare] = Piece.King) then
            moves <- []
            
        moves
        
    member this.CopyBoard() =
        {
            Square = Array.copy this.Square
            ColorToMove = this.ColorToMove
            CastleRights = List.ofSeq this.CastleRights
            EnPassantSquare = this.EnPassantSquare
            Halfmoves = this.Halfmoves
            MoveNumber = this.MoveNumber
        }
        
    member this.CountMaterial(color : Piece) =
        assert (color = Piece.White || color = Piece.Black)
        
        let mutable result = 0
        for piece in this.Square do
            if (isColor piece color) then
                result <- result + Value piece
                  
        result
                
    member this.Evaluate() =
        let whiteEval = this.CountMaterial(Piece.White)
        let blackEval = this.CountMaterial(Piece.Black)
        
        let eval = whiteEval - blackEval
        
        let perspective = if this.ColorToMove = Piece.White then 1 else -1
        
        eval * perspective
        
    member this.IsInCheck() =
        let king = this.Square |> Array.findIndex (fun p -> Piece.King ||| this.ColorToMove = p)
        this.checkAttacked(king)

let init() =
    let square = Seq.replicate 64 Piece.None |> Seq.toArray
    { Square = square
      ColorToMove = Piece.White
      CastleRights = []
      EnPassantSquare = -1
      Halfmoves = 0
      MoveNumber = 1 }
    

    

    
