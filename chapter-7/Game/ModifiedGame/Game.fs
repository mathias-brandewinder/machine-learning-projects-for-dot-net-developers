namespace Game

module Game =

    type Dir = 
        | North
        | West
        | South
        | East

    type Act =
        | Left
        | Right
        | Straight

    type Pos = { Top:int; Left:int }

    type Hero = { Position:Pos; Direction:Dir; }

    type Cell =
        | Treasure
        | Trap

    type Board = int[,]
    
    type GameState = { Board:Board; Hero:Hero; Score:int }

    type Size = { Width:int; Height:int }

    let inline (%%%) (x:int) (y:int) =
        if x >= 0 then x % y
        else y + (x % y)

    let onboard (size:Size) (pos:Pos) =
        { Top = pos.Top %%% size.Height;
          Left = pos.Left %%% size.Width; }

    let moveTo (size:Size) (dir:Dir) (pos:Pos) =
        match dir with
        | North -> { pos with Top = (pos.Top - 1) %%% size.Height }
        | South -> { pos with Top = (pos.Top + 1) %%% size.Height }
        | West -> { pos with Left = (pos.Left - 1) %%% size.Width }
        | East -> { pos with Left = (pos.Left + 1) %%% size.Width}

    let takeDirection (act:Act) (dir:Dir) =
        match act with
        | Straight -> dir
        | Left ->
            match dir with
            | North -> East
            | East -> South
            | South -> West
            | West -> North
        | Right ->
            match dir with
            | North -> West
            | West -> South
            | South -> East
            | East -> North

    let applyDecision (size:Size) (action:Act) (hero:Hero) =
        let newDirection = hero.Direction |> takeDirection action
        { Position = hero.Position |> moveTo size newDirection; Direction = newDirection }

    let tileValues = [| -100; -50; 50; 100 |]

    let computeGain (board:Board) (hero:Hero) =
        let cellType = board.[hero.Position.Left,hero.Position.Top]
        tileValues.[cellType]

    let rng = System.Random ()

    let updateBoard (board:Board) (hero:Hero) =
        let board = board |> Array2D.copy
        let pos = hero.Position
        board.[pos.Left,pos.Top] <- rng.Next(tileValues.Length)
        board