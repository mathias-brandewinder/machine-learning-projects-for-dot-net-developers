#I @"../../../packages"
#r @"Accord/lib/net45/Accord.dll"
#r @"Accord.MachineLearning/lib/net45/Accord.MachineLearning.dll"
#r @"Accord.Math/lib/net45/Accord.Math.dll"
#r @"Accord.Statistics/lib/net45/Accord.Statistics.dll"

open Accord.Statistics.Models.Regression
open Accord.Statistics.Models.Regression.Fitting

let readLogistic fileName =
    let path = __SOURCE_DIRECTORY__ + @"../../Data/" + fileName
    path
    |> System.IO.File.ReadAllLines
    |> fun lines -> lines.[1..]
    |> Array.map (fun line ->
        let parsed = line.Split ',' |> Array.map float
        parsed.[0], parsed.[1..])

let training = readLogistic "trainingsample.csv"
let validation = readLogistic "validationsample.csv"

let labeler x = 
    match x with
    | 4. -> 0.
    | 9. -> 1.
    | _ -> failwith "unexpected label"

let fours = training |> Array.filter (fun (label,_) -> label = 4.)
let nines = training |> Array.filter (fun (label,_) -> label = 9.)

let labels,images = 
    Array.append fours nines 
    |> Array.map (fun (label,image) -> labeler label,image) 
    |> Array.unzip

let features = 28 * 28
let model = LogisticRegression(features)

let trainLogistic (model) =
    let learner = LogisticGradientDescent(model)
    let minDelta = 0.001
    let rec improve () =
        let delta = learner.Run(images,labels)
        printfn "%.4f" delta
        if delta > minDelta
        then improve ()
        else ignore ()
    improve ()

trainLogistic model |> ignore 

let accuracy () =
    validation
    |> Array.filter (fun (label,_) -> label = 4. || label = 9.)
    |> Array.map (fun (label,image) -> labeler label,image)
    |> Array.map (fun (label,image) ->
        let predicted = if model.Compute(image) > 0.5 then 1. else 0.
        let real = label
        if predicted = real then 1. else 0.)
    |> Array.average

accuracy ()

let one_vs_all () =

    let features = 28 * 28
    let labels = [ 0.0 .. 9.0 ]

    let models = 
        labels
        |> List.map (fun target ->
            printfn "Learning label %.0f" target
            // create training set for target label
            let trainingLabels,trainingFeatures = 
                training 
                |> Array.map (fun (label,features) -> 
                    if label = target 
                    then (1.,features) 
                    else (0.,features))
                |> Array.unzip
            // train the model
            let model = LogisticRegression(features)
            let learner = LogisticGradientDescent(model)
            let minDelta = 0.001
            let max_iters = 1000
            let rec improve iter =
                if iter = max_iters 
                then ignore ()
                else
                    let delta = learner.Run(trainingFeatures,trainingLabels)
                    if delta < minDelta then ignore ()
                    else improve (iter + 1)
            improve 0
            // return the label and corresponding model
            target,model)

    let classifier (image:float[]) = 
        models 
        |> List.maxBy (fun (label,model) -> model.Compute image)
        |> fun (label,confidence) -> label
    
    classifier

let accuracy_one_vs_all =
    let model = one_vs_all ()
    validation
    |> Array.map (fun (label,image) ->
        let predicted = model(image)
        let real = label
        if predicted = real then 1. else 0.)
    |> Array.average