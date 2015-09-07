#I @"../../packages"
#r @"FSharp.Data/lib/net40/FSharp.Data.dll"

open FSharp.Data

type Data = CsvProvider<"day.csv">
let dataset = Data.Load("day.csv")
let data = dataset.Rows

// basic statistics

let count = 
    data |> Seq.map (fun x -> float x.Cnt)

let avg = count |> Seq.average
let min = count |> Seq.min
let max = count |> Seq.max

let baseline = 
    let avg = data |> Seq.averageBy (fun x -> float x.Cnt)
    data |> Seq.averageBy (fun x -> abs (float x.Cnt - avg))

// Basic plots
#load @"FSharp.Charting/FSharp.Charting.fsx"
open FSharp.Charting

let all = Chart.Line [ for obs in data -> obs.Cnt ]

// combine 2 line charts
let detail =
  Chart.Combine
    [ Chart.Line [ for obs in data -> obs.Casual ]
      Chart.Line [ for obs in data -> obs.Registered ] ]

// scatterplot of registered vs casual users
let reg_vs_cas = 
    [ for obs in data -> obs.Registered, obs.Casual ] 
    |> Chart.Point

// denoising with moving average

let windowedExample =
    [ 1 .. 10 ]
    |> Seq.windowed 3
    |> Seq.toList

let ma n (series:float seq) = 
    series 
    |> Seq.windowed n
    |> Seq.map (fun xs -> xs |> Seq.average)
    |> Seq.toList

Chart.Combine [
    Chart.Line count
    Chart.Line (ma 7 count)
    Chart.Line (ma 30 count) ]

(*
Basic model: fitting a straight line
*)

type Obs = Data.Row

let model (theta0, theta1) (obs:Obs) = 
    theta0 + theta1 * (float obs.Instant)

let model0 = model (4504., 0.)
let model1 = model (6000., -4.5)

Chart.Combine [
    Chart.Line count
    Chart.Line [ for obs in data -> model0 obs ]
    Chart.Line [ for obs in data -> model1 obs ] ]

type Model = Obs -> float

let cost (data:Obs seq) (m:Model) =
    data 
    |> Seq.sumBy (fun x -> pown (float x.Cnt - m x) 2)
    |> sqrt

let overallCost = cost data
overallCost model0 |> printfn "Cost model0: %.0f" 
overallCost model1 |> printfn "Cost model1: %.0f" 

(*
Gradient descent
*)

let update alpha (theta0, theta1) (obs:Obs) =
    let y = float obs.Cnt
    let x = float obs.Instant
    let theta0' = theta0 - 2. * alpha * 1. * (theta0 + theta1 * x - y)
    let theta1' = theta1 - 2. * alpha * x *  (theta0 + theta1 * x - y)
    theta0', theta1'

let obs100 = data |> Seq.nth 100
let testUpdate = update 0.00001 (0.,0.) obs100
cost [obs100] (model (0.,0.))
cost [obs100] (model testUpdate)

let stochastic rate (theta0,theta1) = 
    data 
    |> Seq.fold (fun (t0,t1) obs -> 
        printfn "%.4f,%.4f" t0 t1
        update rate (t0,t1) obs) (theta0,theta1)

let tune_rate = 
    [ for r in 1 .. 20 -> 
        (pown 0.1 r), stochastic (pown 0.1 r) (0.0,0.0) |> model |> overallCost ]

let rate = pown 0.1 8
let model2 = model (stochastic rate (0.0,0.0))

Chart.Combine [
    Chart.Line count
    Chart.Line [ for obs in data -> model2 obs ] ]


overallCost model2
overallCost model1

let hiRate = 10.0 * rate
let error_eval =
    data 
    |> Seq.scan (fun (t0,t1) obs -> update hiRate (t0,t1) obs) (0.0,0.0)
    |> Seq.map (model >> overallCost)
    |> Chart.Line

let batchUpdate rate (theta0, theta1) (data:Obs seq) =
    let updates = 
        data 
        |> Seq.map (update rate (theta0, theta1))
    let theta0' = updates |> Seq.averageBy fst
    let theta1' = updates |> Seq.averageBy snd    
    theta0', theta1'

let batch rate iters =
    let rec search (t0,t1) i =
        if i = 0 then (t0,t1)
        else
            search (batchUpdate rate (t0,t1) data) (i-1)
    search (0.0,0.0) iters

[ for r in 1 .. 10 -> batch (pown 0.1 r) 100 |> model |> overallCost ]

let better = model (batch (pown 0.1 6) 10000)
overallCost better
overallCost model1
overallCost model2

Chart.Combine [
    Chart.Line count
    Chart.Line [ for obs in data -> better obs ] ]

let batched_error rate =
    Seq.unfold (fun (t0,t1) -> 
        let (t0',t1') = batchUpdate rate (t0,t1) data
        let err = model (t0,t1) |> overallCost
        Some(err, (t0',t1'))) (0.0,0.0)
    |> Seq.take 100
    |> Seq.toList
    |> Chart.Line

batched_error 0.000001