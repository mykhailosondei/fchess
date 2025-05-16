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
    
let (|Move|NotMove|) (pattern: string) =
    let moveRegex = @"^([a-h][1-8]){2}$"
    if Regex.IsMatch(pattern, moveRegex) then Move else NotMove


let position(state, fen) =
    match fen with
    | "startpos" -> Result.Ok { state with Board = parseFEN startPosFEN}
    | FEN -> Result.Ok { state with Board = parseFEN fen}
    | NotFEN -> Result.Error("Invalid FEN")
    
let displayMoves (state:Game) =
    state.moves <- state.GenerateMoves()
    state.moves |> List.iter (fun el -> printfn $"{indexToCell el.StartSquare} {indexToCell el.EndSquare} {el.MoveFlag}")
    Result.Ok state
    
let makeMove(state : Game, move: Move) =
    state.Board.MakePly(Move(move))
    
    Result.Ok state
   
let handleMove(state, move) =
    match move with
    | Move -> 
        let startSquare = cellToIndex(move.Substring(0,2))
        let endSquare = cellToIndex(move.Substring(2,2))
        let move = {StartSquare = startSquare; EndSquare = endSquare; MoveFlag = MoveFlag.None}
        makeMove(state, move)
    | NotMove -> Result.Error "Incorrect move format"
    
        
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
    
        
