namespace Game

open System
open System.Threading
open Game
open Game.Rendering
open Game.Brains

module Program =

    let size = { Width = 50; Height = 30 }
    let hero = 
        {   Position = { Top = size.Height / 2; Left = size.Width / 2 }; 
            Direction = North }
    let board = Array2D.init size.Width size.Height (fun left top -> 
        rng.Next(tileValues.Length))

    let score = 0
    let initialGameState = { Board = board; Hero = hero; Score = score }

    [<EntryPoint>]
    let main argv = 
                      
        let rec loop (state:GameState, brain:Brain) =

            let current = visibleState size state.Board state.Hero
            let decision = Brains.decide brain current // Brains.decide brain current // Brains.randomDecide ()
            
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
             
            // world rendering
            renderScore score
            render state updated

            Thread.Sleep 30
            loop (updated,brain)

        prepareDisplay size initialGameState
        loop (initialGameState,Map.empty)

        0 // return an integer exit code