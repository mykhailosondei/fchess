module fchess.CLI.Commands.ICommandStrategy

open fchess.Core.Game

type ICommandStrategy =
    abstract member Execute: state: Game * args: string -> Result<Game, string> 