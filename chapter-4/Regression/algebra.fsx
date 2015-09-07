#I @"../../../packages"
#r @"FSharp.Data/lib/net40/FSharp.Data.dll"
#load @"FSharp.Charting/FSharp.Charting.fsx"
#r @"MathNet.Numerics.Signed/lib/net40/MathNet.Numerics.dll"
#r @"MathNet.Numerics.FSharp.Signed/lib/net40/MathNet.Numerics.FSharp.dll"

open FSharp.Charting
open FSharp.Data
open MathNet
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Double

(*
Validate that algebra model is doing
the same thing as the previous model
*)


let A = vector [ 1.; 2.; 3. ]
let B = matrix [ [ 1.; 2. ]
                 [ 3.; 4. ]
                 [ 5.; 6. ] ]

let C = A * A
let D = A * B
let E = A * B.Column(1)

type Data = CsvProvider<"day.csv">
let dataset = Data.Load("day.csv")
let data = dataset.Rows

type Vec = Vector<float>
type Mat = Matrix<float>

let cost (theta:Vec) (Y:Vec) (X:Mat) =
    let ps = Y - (theta * X.Transpose())
    ps * ps |> sqrt

let predict (theta:Vec) (v:Vec) = theta * v 

let X = matrix [ for obs in data -> [ 1.; float obs.Instant ]]
let Y = vector [ for obs in data -> float obs.Cnt ]

let theta = vector [6000.; -4.5]

predict theta (X.Row(0))
cost theta Y X 

#time
let theta_normal = (X.Transpose() * X).Inverse() * X.Transpose() * Y

(*
Adding MKL provider for faster linear algebra
*)

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

open MathNet.Numerics
open MathNet.Numerics.Providers.LinearAlgebra.Mkl
Control.LinearAlgebraProvider <- MklLinearAlgebraProvider()

(*
Preparing for cross validation
*)

let seed = 314159
let rng = System.Random(seed)

// Fischer-Yates shuffle
let shuffle (arr:'a []) =
    let arr = Array.copy arr
    let l = arr.Length
    for i in (l-1) .. -1 .. 1 do
        let temp = arr.[i]
        let j = rng.Next(0,i+1)
        arr.[i] <- arr.[j]
        arr.[j] <- temp
    arr

let myArray = [| 1 .. 5 |]
myArray |> shuffle

let training,validation =
    let shuffled =
        data
        |> Seq.toArray
        |> shuffle
    let size = 
        0.7 * float (Array.length shuffled) |> int
    shuffled.[..size],
    shuffled.[size+1..]

(* 
Using normal form for estimation
*)

let estimate (Y:Vec) (X:Mat) =
    (X.Transpose() * X).Inverse() * X.Transpose() * Y

let thetaCompare = estimate Y X

type Obs = Data.Row
type Model = Obs -> float
type Featurizer = Obs -> float list

let predictor (f:Featurizer) (theta:Vec) =
    f >> vector >> (*) theta

let evaluate (model:Model) (data:Obs seq) =
    data 
    |> Seq.averageBy (fun obs -> 
        abs (model obs - float obs.Cnt))

let model (f:Featurizer) (data:Obs seq) =
    let Yt, Xt = 
        data
        |> Seq.toList
        |> List.map (fun obs -> float obs.Cnt, f obs)
        |> List.unzip
    let theta = estimate (vector Yt) (matrix Xt)
    let predict = predictor f theta
    theta,predict

let featurizer0 (obs:Obs) = 
    [   1.; 
        float obs.Instant; ]

let (theta0,model0) = model featurizer0 training

Chart.Combine [
    Chart.Line [ for obs in data -> float obs.Cnt ]
    Chart.Line [ for obs in data -> model0 obs ] ]

evaluate model0 training |> printfn "Training: %.0f"
evaluate model0 validation |> printfn "Validation: %.0f"

(*
Expanded model
*)

let featurizer1 (obs:Obs) =
    [   1.
        obs.Instant |> float
        obs.Atemp |> float
        obs.Hum |> float
        obs.Temp |> float
        obs.Windspeed |> float
    ]

let (theta1,model1) = model featurizer1 training

evaluate model1 training |> printfn "Training: %.0f"
evaluate model1 validation |> printfn "Validation: %.0f"

Chart.Combine [
    Chart.Line [ for obs in data -> float obs.Cnt ]
    Chart.Line [ for obs in data -> model0 obs ]
    Chart.Line [ for obs in data -> model1 obs ] ]

Chart.Point [ for obs in data -> float obs.Cnt, model1 obs ]


(*
Transforming categoricals into features
*)

let featurizer2 (obs:Obs) =
    [   1.
        obs.Instant |> float
        obs.Hum |> float
        obs.Temp |> float
        obs.Windspeed |> float
        (if obs.Weekday = 1 then 1.0 else 0.0)
        (if obs.Weekday = 2 then 1.0 else 0.0)
        (if obs.Weekday = 3 then 1.0 else 0.0)
        (if obs.Weekday = 4 then 1.0 else 0.0)
        (if obs.Weekday = 5 then 1.0 else 0.0)
        (if obs.Weekday = 6 then 1.0 else 0.0)
    ]

let (theta2,model2) = model featurizer2 training

evaluate model2 training |> printfn "Training: %.0f"
evaluate model2 validation |> printfn "Validation: %.0f"

(*
Non linear features
*)

Chart.Point [ for obs in data -> obs.Temp, obs.Cnt ]

let squareTempFeaturizer (obs:Obs) =
    [   1.
        obs.Temp |> float
        obs.Temp * obs.Temp |> float ]

let (_,squareTempModel) = model squareTempFeaturizer data

Chart.Combine [
    Chart.Point [ for obs in data -> obs.Temp, obs.Cnt ]
    Chart.Point [ for obs in data -> obs.Temp, squareTempModel obs ] ]

let featurizer3 (obs:Obs) =
    [   1.
        obs.Instant |> float
        obs.Hum |> float
        obs.Temp |> float
        obs.Windspeed |> float
        obs.Temp * obs.Temp |> float
        (if obs.Weekday = 1 then 1.0 else 0.0)
        (if obs.Weekday = 2 then 1.0 else 0.0)
        (if obs.Weekday = 3 then 1.0 else 0.0)
        (if obs.Weekday = 4 then 1.0 else 0.0)
        (if obs.Weekday = 5 then 1.0 else 0.0)
        (if obs.Weekday = 6 then 1.0 else 0.0)
    ]

let (theta3,model3) = model featurizer3 training

evaluate model3 training |> printfn "Training: %.0f"
evaluate model3 validation |> printfn "Validation: %.0f"

Chart.Combine [
    Chart.Line [ for obs in data -> float obs.Cnt ]
    Chart.Line [ for obs in data -> model0 obs ]
    Chart.Line [ for obs in data -> model3 obs ] ]

Chart.Point [ for obs in data -> float obs.Cnt, model3 obs ]
Chart.Point [ for obs in validation -> float obs.Cnt, model3 obs ]