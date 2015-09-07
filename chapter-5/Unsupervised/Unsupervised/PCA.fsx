(*
Principal Components Analysis
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
#r @"../packages/MathNet.Numerics.FSharp.Signed.3.6.0/lib/net40/MathNet.Numerics.FSharp.dll"

open MathNet
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.Statistics

let correlations = 
    observations 
    |> Matrix.Build.DenseOfColumnArrays 
    |> Matrix.toRowArrays 
    |> Correlation.PearsonMatrix

let feats = headers.Length
let correlated = 
    [ 
        for col in 0 .. (feats - 1) do
            for row in (col + 1) .. (feats - 1) ->
                correlations.[col,row], headers.[col], headers.[row]
    ]
    |> Seq.sortBy (fun (corr, f1, f2) -> - abs corr)
    |> Seq.take 100
    |> Seq.iter (fun (corr, f1, f2) -> 
        printfn "%s %s : %.2f" f1 f2 corr)


#load "PCA.fs"
open Unsupervised.PCA

// Center & Normalize
let normalized = normalize (headers.Length) observations

// Run PCA
let (eValues,eVectors), projector = pca normalized
    
// How much information is captured by each feature?
let total = eValues |> Seq.sumBy (fun x -> x.Magnitude)
eValues
|> Vector.toList
|> List.rev
|> List.scan (fun (percent,cumul) value -> 
    let percent = 100. * value.Magnitude / total
    let cumul = cumul + percent
    (percent,cumul)) (0.,0.)
|> List.tail
|> List.iteri (fun i (p,c) -> printfn "Feat %2i: %.2f%% (%.2f%%)" i p c)

// project the original features 
// on each of the new components 
let principalComponents comp1 comp2 =
    let title = sprintf "Component %i vs %i" comp1 comp2
    let features = headers.Length
    let coords = Seq.zip (eVectors.Column(features-comp1)) (eVectors.Column(features-comp2))
    Chart.Point (coords, Title = title, Labels = headers, MarkerSize = 7) 
    |> Chart.WithXAxis(Min = -1.0, Max = 1.0, 
        MajorGrid = ChartTypes.Grid(Interval = 0.25), 
        LabelStyle = ChartTypes.LabelStyle(Interval = 0.25), 
        MajorTickMark = ChartTypes.TickMark(Enabled = false))
    |> Chart.WithYAxis(Min = -1.0, Max = 1.0, 
        MajorGrid = ChartTypes.Grid(Interval = 0.25), 
        LabelStyle = ChartTypes.LabelStyle(Interval = 0.25), 
        MajorTickMark = ChartTypes.TickMark(Enabled = false))

// project the users on the new features
let projections comp1 comp2 =
    let title = sprintf "Component %i vs %i" comp1 comp2
    let features = headers.Length
    let coords = 
        normalized 
        |> Seq.map projector
        |> Seq.map (fun obs -> obs.[features-comp1], obs.[features-comp2])
    Chart.Point (coords, Title = title)
    |> Chart.WithXAxis(Min = -200.0, Max = 500.0,
        MajorGrid = ChartTypes.Grid(Interval = 100.), 
        LabelStyle = ChartTypes.LabelStyle(Interval = 100.), 
        MajorTickMark = ChartTypes.TickMark(Enabled = false))
    |> Chart.WithYAxis(Min = -200.0, Max = 500.0,
        MajorGrid = ChartTypes.Grid(Interval = 100.), 
        LabelStyle = ChartTypes.LabelStyle(Interval = 100.), 
        MajorTickMark = ChartTypes.TickMark(Enabled = false))


[ for i in 1 .. 4 do
    for j in (i+1) .. 5 ->
        principalComponents i j ]

[ for i in 1 .. 4 do
    for j in (i+1) .. 5 ->
        projections i j ]
