#load "Game.fs"
open Game.Game
#load "Brains.fs"
open Game.Brains
open System

let size = { Width = 40; Height = 20 }
let hero = { Position = { Top = 10; Left = 20 }; Direction = North }

let rng = Random ()

let board = Array2D.init size.Width size.Height (fun left top -> 
    rng.Next(tileValues.Length))

let score = 0
let initial = { Board = board; Hero = hero; Score = score }

let simulate (decide:Brain -> State -> Act) iters runs =

    let rec loop (state:GameState,brain:Brain,iter:int) =

        let current = visibleState size state.Board state.Hero
        let decision = decide brain current
            
        // world update
        let hero = state.Hero |> applyDecision size decision
        let board = updateBoard state.Board hero
        let gain = computeGain state.Board hero
        let score = state.Score + gain
             
        // learning

        let result = visibleState size board hero
        let experience = 
            {   State = current; 
                Action = decision; 
                Reward = gain |> float; 
                NextState = result }
        let brain = learn brain experience

        let updated = { Board = board; Hero = hero; Score = score }
        if iter < iters
        then loop (updated,brain,iter+1)
        else score

    [ for run in 1 .. runs -> loop (initial,Map.empty,0) ]

printfn "Random decision"
let random = simulate (fun _ _ -> Game.Brains.randomDecide ()) 100000 20
printfn "average: %.0f" (random |> Seq.averageBy float)
printfn "Better brain"
let betterBrain = simulate Game.Brains.decide 100000 20
printfn "average: %.0f" (betterBrain |> Seq.averageBy float)