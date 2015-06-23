#load "Game.fs"
open Game.Game
#load "Brains.fs"
open Game.Brains
open System

let size = { Width = 40; Height = 20 }
let player = { Position = { Top = 10; Left = 20 }; Direction = North }

let rng = Random ()

let board = 
    [   for top in 0 .. size.Height - 1 do
            for left in 0 .. size.Width - 1 do
                if rng.NextDouble () > 0.5
                then 
                    let pos = { Top = top; Left = left }
                    let cell = if rng.NextDouble () > 0.5 then Trap else Treasure
                    yield pos, cell ]
    |> Map.ofList

let score = 0
let initial = { Board = board; Hero = player; Score = score }

let simulate (decide:Brain -> State -> Act) iters runs =

    let rec loop (state:GameState,brain:Brain,iter:int) =

        let current = visibleState size state.Board state.Hero
        let decision = decide brain current
            
        // world update
        let player = state.Hero |> applyDecision size decision
        let board = updateBoard state.Board player
        let gain = computeGain state.Board player
        let score = state.Score + gain
             
        // learning

        let result = visibleState size board player
        let experience = 
            {   State = current; 
                Action = decision; 
                Reward = gain |> float; 
                NextState = result }
        let brain = learn brain experience

        let updated = { Board = board; Hero = player; Score = score }
        if iter < iters
        then loop (updated,brain,iter+1)
        else score

    [ for run in 1 .. runs -> loop (initial,Map.empty,0) ]

printfn "Random decision"
let random = simulate (fun _ _ -> Game.Brains.randomDecide ()) 500 20
printfn "average: %.0f" (random |> Seq.averageBy float)
printfn "Crude brain"
let crudeBrain = simulate Game.Brains.decide 500 20
printfn "average: %.0f" (crudeBrain |> Seq.averageBy float)