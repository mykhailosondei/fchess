module fchess.Core.Board

open Piece
open fchess.Core.Move

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
            endSquarePiece <- Piece.Rook
        | MoveFlag.PromoteToQueen ->
            assert (getPieceType this.Square[move.StartSquare] = Piece.Pawn)
            endSquarePiece <- Piece.Queen
        | MoveFlag.PromoteToKnight ->
            assert (getPieceType this.Square[move.StartSquare] = Piece.Pawn)
            endSquarePiece <- Piece.Knight
        | MoveFlag.PromoteToBishop ->
            assert (getPieceType this.Square[move.StartSquare] = Piece.Pawn)
            endSquarePiece <- Piece.Bishop
        | _ ->
            endSquarePiece <- this.Square[move.StartSquare]
            
        
        if specialty move.MoveFlag = MoveFlag.DoublePawnMove then
            assert (getPieceType this.Square[move.StartSquare] = Piece.Pawn)
            if Piece.Pawn ||| oppositeColor this.ColorToMove = this.Square[move.EndSquare+1] ||
               Piece.Pawn ||| oppositeColor this.ColorToMove = this.Square[move.EndSquare-1]
            then
                this.EnPassantSquare <- (move.StartSquare + move.EndSquare) / 2
        else this.EnPassantSquare <- -1
            
        
        match specialty move.MoveFlag with
        | MoveFlag.KingMove ->
            assert (getPieceType this.Square[move.StartSquare] = Piece.King)
            this.CastleRights <- this.CastleRights |> List.where (fun x -> (isColor x <| oppositeColor this.ColorToMove))
        | MoveFlag.QueenRookMove ->
            assert (getPieceType this.Square[move.StartSquare] = Piece.Rook)
            this.CastleRights <- this.CastleRights |> List.where (fun right -> not (Piece.Queen ||| this.ColorToMove = right))
        | MoveFlag.KingRookMove ->
            assert (getPieceType this.Square[move.StartSquare] = Piece.Rook)
            this.CastleRights <- this.CastleRights |> List.where (fun right -> not (Piece.King |||  this.ColorToMove = right))
        | _ -> ()
        
        if specialty move.MoveFlag = MoveFlag.EnPassant then
            assert (move.EndSquare = this.EnPassantSquare)
            assert (getPieceType this.Square[move.StartSquare] = Piece.Pawn)
            let pawnShift = move.EndSquare % 8 - move.StartSquare % 8
            this.Square[move.StartSquare + pawnShift] <- Piece.None
            
        this.Square[move.StartSquare] <- Piece.None
        this.Square[move.EndSquare] <- endSquarePiece
       
        this.Halfmoves <- this.Halfmoves + 1
        
        if breaksRule move.MoveFlag then
            this.Halfmoves <- 0
        
        
        if this.ColorToMove = Piece.Black then
            this.MoveNumber <- this.MoveNumber + 1
        
        this.ColorToMove <- oppositeColor this.ColorToMove            
            
       
        
       
let init() =
    let square = Seq.replicate 64 Piece.None |> Seq.toArray
    { Square = square
      ColorToMove = Piece.White
      CastleRights = []
      EnPassantSquare = -1
      Halfmoves = 0
      MoveNumber = 1 }
    

    

    
