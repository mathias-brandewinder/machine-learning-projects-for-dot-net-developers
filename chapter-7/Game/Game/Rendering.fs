namespace Game

open System
open Game

module Rendering =

    let offset (pos:Pos) = (pos.Left, pos.Top + 2)
    let writeAt (left,top) color (txt:string) = 
        Console.ForegroundColor <- color
        Console.SetCursorPosition (left,top)
        Console.Write txt

    let prepareDisplay size =
        Console.SetWindowSize(size.Width, size.Height+2)

    let renderPlayer (before:Hero) (after:Hero) =
        writeAt (offset (before.Position)) ConsoleColor.Black "█"
        writeAt (offset (after.Position)) ConsoleColor.Yellow "█"

    let renderBoard (before:Board) (after:Board) =
        after
        |> Map.iter (fun pos item -> 
            if (before |> Map.containsKey pos)
            then
                match item with
                | Treasure -> 
                    writeAt (offset pos) ConsoleColor.Blue "@"
                | Trap -> 
                    writeAt (offset pos) ConsoleColor.Red "+"
            else writeAt (offset pos) ConsoleColor.Black " ") 

    let renderScore score = 
        writeAt (0,0) ConsoleColor.White (sprintf "Score: %i   " score)