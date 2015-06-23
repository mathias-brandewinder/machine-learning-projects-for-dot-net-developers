namespace Game

open System
open Game

module Rendering =

    let colors = 
        [|
            ConsoleColor.DarkRed
            ConsoleColor.Red
            ConsoleColor.DarkYellow
            ConsoleColor.Yellow
        |]

    let creatureColor = ConsoleColor.White

    let offset (pos:Pos) = (pos.Left, pos.Top + 2)

    let prepareDisplay size gameState =
        Console.SetWindowSize(size.Width, size.Height+2)
        let board = gameState.Board
        for x in 0 .. (size.Width - 1) do
            for y in 0 .. (size.Height - 1) do
                let pos = { Left = x; Top = y }
                Console.SetCursorPosition (offset (pos))
                let tileType = board.[x,y]
                Console.ForegroundColor <- colors.[tileType]
                Console.Write("█")               

    let render (before:GameState) (after:GameState) =
        let oldPos = before.Hero.Position
        let newPos = after.Hero.Position
        // previous player position
        Console.SetCursorPosition (offset (oldPos))
        let tileType = after.Board.[oldPos.Left,oldPos.Top]
        Console.ForegroundColor <- colors.[tileType]
        Console.Write("█")
        // current player position
        Console.SetCursorPosition (offset (newPos))
        Console.ForegroundColor <- creatureColor
        Console.Write("█")

    let renderScore score = 
        Console.ForegroundColor <- ConsoleColor.White
        Console.SetCursorPosition (0,0)
        Console.Write (sprintf "Score: %i   " score)