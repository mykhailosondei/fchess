﻿module fchess_Tests

open NUnit.Framework

open fchess.Core
open FsUnit
open fchess.Core.Move
open fchess.Core.Piece
open fchess.Core.Utils

let private startPosFEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

let private startPosFEN_source() =seq{ startPosFEN }

[<SetUp>]
let Setup () =
    ()

[<Test>]
let TestBoardMakePly_Pawn () =
    let board = parseFEN startPosFEN
    let move : Move = {StartSquare = 8; EndSquare = 16; MoveFlag = MoveFlag.PawnMove}
    
    board.MakePly(Move(move))
    
    let expected = parseFEN "rnbqkbnr/pppppppp/8/8/8/P7/1PPPPPPP/RNBQKBNR b KQkq - 0 1"
    
    printfn "%s" (board.ToString())
    printfn "%s" (expected.ToString())
    
    board |> should equal expected
        
[<Test>]
let TestBoardMakePly_Castle () =
    let board = parseFEN "rnbqkb1r/ppp2ppp/3p1n2/4p3/2B1P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 0 4"
    let castle = Castle.Kingside
    
    board.MakePly(Castle(castle))
    
    let expected = parseFEN "rnbqkb1r/ppp2ppp/3p1n2/4p3/2B1P3/5N2/PPPP1PPP/RNBQ1RK1 b kq - 1 4"
    
    printfn "%s" (board.ToString())
    printfn "%s" (expected.ToString())
    
    board |> should equal expected 
    
[<Test>]
let TestBoardMakeCastle_Queenside () =
    let board = parseFEN "rnbqkbnr/ppp2ppp/8/3p4/3P4/2NpB3/PPP1PPPP/R3KBNR w KQkq - 0 5"
    let castle = Castle.Queenside
    
    board.MakeCastle(castle)
    
    let expected = parseFEN "rnbqkbnr/ppp2ppp/8/3p4/3P4/2NpB3/PPP1PPPP/2KR1BNR b kq - 1 5"
    
    printfn "%s" (board.ToString())
    printfn "%s" (expected.ToString())
    
    board |> should equal expected
    
    
[<Test>]
let TestBoardMakeCastle_Kingside () =
    let board = parseFEN "rnbqkbnr/pppp2pp/8/4p3/5p2/5NPB/PPPPPP1P/RNBQK2R w KQkq - 0 4"
    let castle = Castle.Kingside
    
    board.MakeCastle(castle)
    
    let expected = parseFEN "rnbqkbnr/pppp2pp/8/4p3/5p2/5NPB/PPPPPP1P/RNBQ1RK1 b kq - 1 4"
    
    printfn "%s" (board.ToString())
    printfn "%s" (expected.ToString())
    
    board |> should equal expected
    
[<Test>]
let TestBoardMakeMove_Sequence () =
    let board = parseFEN "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2"
    let Ng3 : Move = {StartSquare = 6; EndSquare = 21; MoveFlag = MoveFlag.None}
    let ng6 : Move = {StartSquare = 62; EndSquare = 45; MoveFlag = MoveFlag.None}
    
    board.MakeMove(Ng3)
    board.MakeMove(ng6)
    
    let expected = parseFEN "rnbqkb1r/pppp1ppp/5n2/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 2 3"
    
    printfn "%s" (board.ToString())
    printfn "%s" (expected.ToString())
    
    board |> should equal expected
    
    
[<Test>]
let TestBoardMakeMove_DoublePawnMove_ShouldHaveEnPassant () =
    let board = parseFEN "rnbqkbnr/ppp1pppp/8/8/3p4/1P1P4/P1P1PPPP/RNBQKBNR w KQkq - 0 3"
    let e4 : Move = {StartSquare = 12; EndSquare = 28; MoveFlag = MoveFlag.DoublePawnMove ||| MoveFlag.PawnMove}
    
    board.MakeMove(e4)
    
    let expected = parseFEN "rnbqkbnr/ppp1pppp/8/8/3pP3/1P1P4/P1P2PPP/RNBQKBNR b KQkq e3 0 3"
    
    printfn "%s" (board.ToString())
    printfn "%s" (expected.ToString())
    
    board |> should equal expected

[<Test>]
let TestIsColor_White () =
    let piece = (Piece.White ||| Piece.King)
    
    let result = isColor piece Piece.White
    
    Assert.That(result)
    
[<Test>]
let TestIsColor_Black () =
    let piece = (Piece.Black ||| Piece.Queen)
    
    let result = isColor piece Piece.Black
    
    Assert.That(result)

[<Test>]
let TestOppositeColor_White () =
    let color = Piece.White
    
    let result = Piece.Black = oppositeColor color
    
    Assert.That(result)
    
[<Test>]
let TestOppositeColor_Black () =
    let color = Piece.Black
    
    let result = Piece.White = oppositeColor color
    
    Assert.That(result)
    
[<Test>]
let TestGetPieceType_King () =
    let piece = (Piece.White ||| Piece.King)
    
    let result = Piece.King = getPieceType piece
    
    Assert.That(result)
    
[<Test>]
let TestGetPieceType_Queen () =
    let piece = (Piece.Black ||| Piece.Queen)
    
    let result = Piece.Queen = getPieceType piece
    
    Assert.That(result)
    
[<Test>]
let TestIsSliding_Bishop () =
    let piece = (Piece.White ||| Piece.Bishop)
    
    let result = isSliding piece
    
    Assert.That(result)
    
[<Test>]
let TestIsSliding_Knight () =
    let piece = (Piece.Black ||| Piece.Knight)
    
    let result = isSliding piece
    
    Assert.That(not result)
    
let stockFishMoveCount fen =
    use process = new System.Diagnostics.Process()
    process.StartInfo.FileName <- "stockfish"
    process.StartInfo.RedirectStandardInput <- true
    process.StartInfo.RedirectStandardOutput <- true
    process.StartInfo.UseShellExecute <- false
    process.Start() |> ignore
    
    process.StandardInput.WriteLine("position fen " + fen)
    process.StandardInput.WriteLine("go perft 1")
    
    let mutable output = ""
    while not (output.Contains("Nodes searched")) do
        output <- process.StandardOutput.ReadLine()
    
    let perftResult = output.Split(':').[1].Trim() |> int
    process.Kill()
    perftResult
    
[<Test>]
[<TestCaseSource("startPosFEN_source")>]
[<TestCase("rnbqkbnr/ppp1pppp/8/8/3pP3/1P1P4/P1P2PPP/RNBQKBNR b KQkq e3 0 3")>]
[<TestCase("r2q1rk1/2p1bppp/p1n1bn2/1p2p3/4P3/2P2N2/PPBN1PPP/R1BQR1K1 w - - 1 12")>]
let TestGenMovesLength_AgainstStockfish_Modular (fen) =
    let board = parseFEN startPosFEN
    
    let game = {Game.init() with Board = board} 
    
    let moveCount = game.Board.GenerateMoves().Length
    
    let stockfishOutput = stockFishMoveCount startPosFEN
    
    let result = stockfishOutput = moveCount 
    
    Assert.That(result)
    
[<Test>]
let TestGenLegalMovesLength_AgainstStockfish_Startpos () =
    let board = parseFEN startPosFEN
    
    let game = {Game.init() with Board = board} 
    
    let moveCount = game.GenerateLegalMoves().Length
    
    let stockfishOutput = stockFishMoveCount startPosFEN
    
    let result = stockfishOutput = moveCount 
    
    Assert.That(result)
    
    
[<Test>]
let TestGenLegalMovesLength_AgainstStockfish_Random () =
    let board = parseFEN "r2q1rk1/2p1bppp/p1n1bn2/1p2p3/4P3/2P2N2/PPBN1PPP/R1BQR1K1 w - - 1 12"
    
    let game = {Game.init() with Board = board}
    
    let moveCount = game.GenerateLegalMoves().Length
    
    let stockfishOutput = stockFishMoveCount "r2q1rk1/2p1bppp/p1n1bn2/1p2p3/4P3/2P2N2/PPBN1PPP/R1BQR1K1 w - - 1 12"
    
    let result = stockfishOutput = moveCount
    
    printfn $"{stockfishOutput}"
    printfn $"{moveCount}"
    
    Assert.That(result)

[<Test>]
let TestGenMovesLength_AgainstStockfish_Random () =
    let fen = "rnbqkb1r/pp1p1ppp/5n2/2p1p3/4P3/3P4/PPP1NPPP/RNBQKB1R w KQkq - 0 4"
    let board = parseFEN fen
    
    let game = {Game.init() with Board = board} 
    
    let moveCount = game.Board.GenerateMoves().Length
    
    let stockfishOutput = stockFishMoveCount fen
    
    let result = stockfishOutput = moveCount 
    
    Assert.That(result)
    
[<Test>]
let TestGenCastlesLength_NoCastlesInFen () =
    let board = parseFEN "rnbqkb1r/4n3/pppppppp/8/8/1PNBPN2/PBPPQPPP/R3K2R w kq - 6 12"
    
    let game = {Game.init() with Board = board} 
   
    let castleCount = game.Board.GenerateCastles().Length
    
    let result = castleCount = 0
    
    Assert.That(result)
    
[<Test>]
let TestGenCastlesLength_2Castles () =
    let board = parseFEN "rnbqkbnr/p7/1ppppppp/8/8/1PNBPN2/PBPPQPPP/R3K2R w KQkq - 0 8"
    
    let game = {Game.init() with Board = board} 
   
    let castleCount = game.Board.GenerateCastles().Length
    
    printfn "%d" castleCount
    
    let result = castleCount = 2
    
    Assert.That(result)

[<Test>]
let TestDetermineFlag_QueensRook_White () =
    let board = parseFEN startPosFEN
    
    let game = {Game.init() with Board = board}
    
    let startSquare = 0 // a1, white queen's rook
    let endSquare = 8   // a2
    let pieceType = Piece.Rook
    
    let flag = game.Board.DetermineFlag(startSquare, endSquare, pieceType)
    
    let result = flag = MoveFlag.QueenRookMove
    
    Assert.That(result)
    
[<Test>]
[<TestCaseSource("startPosFEN_source")>]
let TestDetermineFlag_KingsRook_White (fen) =
    let board = parseFEN fen
    
    let game = {Game.init() with Board = board}
    
    let startSquare = 7 // h1, white king's rook
    let endSquare = 15  // h2
    let pieceType = Piece.Rook
    
    let flag = game.Board.DetermineFlag(startSquare, endSquare, pieceType)
    
    let result = flag = MoveFlag.KingRookMove
    
    Assert.That(result)
    
[<Test>]
[<TestCase("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b KQkq - 0 1")>]
let TestDetermineFlag_QueensRook_Black (fen) =
    let board = parseFEN fen
    
    let game = {Game.init() with Board = board}
    
    let startSquare = 56 // a8, black queen's rook
    let endSquare = 48   // a7
    let pieceType = Piece.Rook
    
    let flag = game.Board.DetermineFlag(startSquare, endSquare, pieceType)
    
    let result = flag = MoveFlag.QueenRookMove
    
    Assert.That(result)
    
[<Test>]
[<TestCase("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b KQkq - 0 1")>]
let TestDetermineFlag_KingsRook_Black (fen) =
    let board = parseFEN fen
    
    let game = {Game.init() with Board = board}
    
    let startSquare = 63 // h8, black king's rook
    let endSquare = 55   // h7
    let pieceType = Piece.Rook
    
    let flag = game.Board.DetermineFlag(startSquare, endSquare, pieceType)
    
    let result = flag = MoveFlag.KingRookMove
    
    Assert.That(result)

[<Test>]
let TestPerft_Depth4_Startpos () =
    let board = parseFEN startPosFEN
    
    let game = {Game.init() with Board = board}
    
    let perft = game.GeneratePerft(4)
    
    perft = 197281 |> Assert.That
    
    