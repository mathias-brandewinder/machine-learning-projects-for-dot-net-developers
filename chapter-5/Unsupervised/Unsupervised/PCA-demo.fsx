(*
Illustrate PCA on simplified dataset
using only 2 highly correlated features.
*)

open System
open System.IO

#r @"../packages/FSharp.Charting/lib/net40/FSharp.Charting.dll"
#load @"../packages/FSharp.Charting/FSharp.Charting.fsx"
open FSharp.Charting

(*
Loading the dataset
*)

let folder = __SOURCE_DIRECTORY__
let file = "userprofiles-toptags.txt"

let headers,observations =

    let raw = 
        folder + "/" + file
        |> File.ReadAllLines

    // first row is headers, first col is user ID
    let headers = (raw.[0].Split ',').[1..]

    let observations =
        raw.[1..]
        |> Array.map (fun line -> (line.Split ',').[1..])
        |> Array.map (Array.map float)
    
    headers,observations

(*
Loading Math.NET and computing basic correlations
*)

#r @"../packages/MathNet.Numerics.Signed/lib/net40/MathNet.Numerics.dll"
#r @"../packages/MathNet.Numerics.FSharp.Signed/lib/net40/MathNet.Numerics.FSharp.dll"

open MathNet
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.Statistics

#load "PCA.fs"
open Unsupervised.PCA

// Center & Normalize
let normalized = normalize (headers.Length) observations


// ios 12
// objective c 20
// 26 sql
// 27 sql server

let simplified = 
    normalized 
    |> Array.map (fun row -> [| row.[12]; row.[20]; row.[26]; row.[27] |])

simplified 
|> Seq.map (fun row -> row.[0], row.[3]) 
|> Chart.Point

let (eValues,eVectors), projector = pca simplified

let mainFeatures =
    let title = sprintf "Component 1 vs. Component 2"
    let coords = Seq.zip (eVectors.Column(3)) (eVectors.Column(2))
    Chart.Point (coords, Title = title, Labels = ["ios";"obj-c";"sql";"sql-server"])
    |> Chart.WithXAxis(Max = 1.0, Min = -1.0, MajorGrid = ChartTypes.Grid(Interval = 0.25), LabelStyle = ChartTypes.LabelStyle(Interval = 0.25), MajorTickMark = ChartTypes.TickMark(Enabled = false))
    |> Chart.WithYAxis(Max = 1.0, Min = -1.0, MajorGrid = ChartTypes.Grid(Interval = 0.25), LabelStyle = ChartTypes.LabelStyle(Interval = 0.25), MajorTickMark = ChartTypes.TickMark(Enabled = false))

let original =
    let title = "Component 0 vs. Component 1"
    let coords = 
        simplified 
        |> Seq.map (fun obs -> obs.[0], obs.[1])
    Chart.Point (coords, Title = title) 
    |> Chart.WithXAxis(Max = 100., Min = -50.)
    |> Chart.WithYAxis(Max = 100., Min = -50.)

let projected =
    let title = "Component 1 vs. Component 2"
    let coords = 
        simplified 
        |> Seq.map projector
        |> Seq.map (fun obs -> obs.[3], obs.[2])
    Chart.Point (coords, Title = title) 
    |> Chart.WithXAxis(Max = 400., Min = -150., MajorGrid = ChartTypes.Grid(Interval = 50.), LabelStyle = ChartTypes.LabelStyle(Interval = 50.), MajorTickMark = ChartTypes.TickMark(Enabled = false))
    |> Chart.WithYAxis(Max = 400., Min = -150., MajorGrid = ChartTypes.Grid(Interval = 50.), LabelStyle = ChartTypes.LabelStyle(Interval = 50.), MajorTickMark = ChartTypes.TickMark(Enabled = false))