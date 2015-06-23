namespace Game

open System
open System.Threading
open Game
open Game.Rendering
open Game.Brains

module Program =

    // world initialization
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
    let initialGameState = { Board = board; Hero = player; Score = score }

    prepareDisplay size

    [<EntryPoint>]
    let main argv = 

        let rec loop (state:GameState,brain:Brain) =

            let currentState = visibleState size state.Board state.Hero 
            let decision = Brains.decide brain currentState
            
            // world update
            let player = state.Hero |> applyDecision size decision
            let board = updateBoard state.Board player
            let gain = computeGain state.Board player
            let score = state.Score + gain

            // learning
            let nextState = visibleState size board player
            let experience = {
                State = currentState;
                Action = decision;
                Reward = gain |> float;
                NextState = nextState; }
            let brain = learn brain experience

            // world rendering
            renderScore score
            renderPlayer state.Hero player
            renderBoard state.Board board

            let updated = { Board = board; Hero = player; Score = score }
            
            Thread.Sleep 20
            loop (updated,brain)

        let _ = loop (initialGameState,Map.empty)

        0 // return an integer exit code