module fchess.CLI.CommandHandler


open System.Text.RegularExpressions
open fchess.Core.Move
open fchess.Core.Game
open fchess.Core.Utils

let startPosFEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

let (|Prefix|_|) (p:string) (s:string) =
    if s.StartsWith(p) then
        Some(s.Substring(p.Length))
    else
        None
        
        
let (|FEN|NotFEN|) (pattern: string) =
    let fenRegex = @"^([rnbqkpRNBQKP1-8]+\/){7}[rnbqkpRNBQKP1-8]+ [wb] (K?Q?k?q?|-){1} ([a-h][36]|-) \d+ \d+$"
    if Regex.IsMatch(pattern, fenRegex) then FEN else NotFEN
    
let toPly (pattern: string) =
    let moveRegex = @"^([a-h][1-8]){2}$"
    let queenside = "O-O-O"
    let kingside = "O-O"
    match pattern with
    | pattern when Regex.IsMatch(pattern, moveRegex) ->
        let startSquare = cellToIndex(pattern.Substring(0,2))
        printfn "%d" startSquare
        let endSquare = cellToIndex(pattern.Substring(2,2))
        printfn "%d" endSquare
        let move = {StartSquare = startSquare; EndSquare = endSquare; MoveFlag = MoveFlag.None}
        Some(Move(move))
    | pattern when pattern = queenside ->
        Some(Castle(Castle.Queenside))
    | pattern when pattern = kingside ->
        Some(Castle(Castle.Kingside))
    | _ -> None


let position(state, fen) =
    match fen with
    | "startpos" -> Result.Ok { state with Board = parseFEN startPosFEN}
    | FEN -> Result.Ok { state with Board = parseFEN fen}
    | NotFEN -> Result.Error("Invalid FEN")
    
let displayMoves (state:Game) =
    state.moves <- state.GenerateMoves()
    state.moves |> List.iter (fun move ->
        printfn $"{indexToCell move.StartSquare} {indexToCell move.EndSquare} {move.MoveFlag}"
        )
    
    state.castles |> List.iter (fun castle ->
        let message = "O-O".Insert(0, if castle.IsKingside then "" else "O-")
        printfn $"%s{message}"
        )
    
    Result.Ok state
    
let makePly(state : Game, ply: Ply) =
    state.Board.MakePly(ply)
    
    Result.Ok state
   
let handleMove(state : Game, moveString) =
    
    match toPly(moveString) with
        | Some(Move move) ->
            let moves = state.GenerateMoves()
            let move' = moves |> List.tryFind (
                fun x -> x.EndSquare = move.EndSquare && x.StartSquare = move.StartSquare
            )
            match move' with
            | Some move -> makePly(state, Move(move))
            | None -> Result.Error("Move is not legal")
        | Some(Castle castle) ->
            let castles = state.GenerateCastles()
            let castle' = castles |> List.tryFind (fun x -> x = castle)
            
            match castle' with
            | Some castle -> makePly(state, Castle(castle))
            | None -> Result.Error("Cannot castle")
        | None -> Result.Error("Invalid move") 
    
    
        
let handle (state: Game, cmd: string) =
    match cmd with
    | Prefix "position " rest -> position(state, rest)
    | Prefix "d" _ ->
        printfn $"{state.Board.ToString()}"
        Result.Ok state
    | Prefix "moves" _ ->
        displayMoves(state)
    | Prefix "move " rest ->       
        handleMove(state, rest)
    | _ -> Result.Error("Unknown command, type \"help\" for the list of available commands.")
    
        
