#!/usr/bin/env MONO_OPTIONS=-O=-aot MONO_GC_DEBUG=nursery-canaries
#r @"../../packages/FSharp.Data/lib/net40/FSharp.Data.dll"
#load "Tree.fs"

open System
open FSharp.Data
open Titanic
open Titanic.Tree

type Titanic = CsvProvider<"titanic.csv">
type Passenger = Titanic.Row

let dataset = Titanic.GetSample ()

let label (p:Passenger) = p.Survived

let features = [
    "Sex", fun (p:Passenger) -> p.Sex |> Some
    "Class", fun p -> p.Pclass |> string |> Some
    "Age", fun p -> if p.Age < 7.0 then Some("Younger") else Some("Older") ]

let tree = growTree dataset.Rows label (features |> Map.ofList)

// evaluate the tree
dataset.Rows
|> Seq.averageBy (fun p -> if p.Survived = decide tree p then 1. else 0.)

display 0 tree


(*
Over-fitting issues
*)

let idExample = [
    "ID", fun (p:Passenger) -> p.PassengerId |> string |> Some ]

let idTree = growTree dataset.Rows label (idExample |> Map.ofList)

dataset.Rows
|> Seq.averageBy (fun p -> if p.Survived = decide idTree p then 1.0 else 0.0)
|> printfn "Correct: %.3f"

idTree |> display 0


// Using filters

let filteredTree = 
    growTree2 [ entropyGainFilter; leafSizeFilter 10 ] dataset.Rows label (features |> Map.ofList)
    |> display 0


(*
Experiment: more and more features, 
comparing results on training and validation. 
*)

let kfold k sample =

    let size = sample |> Array.length
    let foldSize = size / k
    
    [ for f in 0 .. (k-1) do
        let sliceStart = f * foldSize
        let sliceEnd = f * foldSize + foldSize - 1
        let validation = sample.[sliceStart..sliceEnd]
        let training = 
            [| 
                for i in 0 .. (sliceStart - 1) do yield sample.[i] 
                for i in (sliceEnd + 1) .. (size - 1) do yield sample.[i] 
            |]
        yield training,validation
    ]

let folds = dataset.Rows |> Seq.toArray |> kfold 10
let accuracy tree (sample:Passenger seq) = 
    sample 
    |> Seq.averageBy (fun p -> 
        if p.Survived = decide tree p then 1.0 else 0.0)

let evaluateFolds =
    let filters = [ leafSizeFilter 10; entropyGainFilter ]
    let features = features |> Map.ofList
    [for (training,validation) in folds ->        
        let tree = growTree2 filters training label features
        let accuracyTraining = accuracy tree training
        let accuracyValidation = accuracy tree validation

        printfn "Training: %.3f, Validation: %.3f" accuracyTraining accuracyValidation
        accuracyTraining, accuracyValidation]

// average on training part    
evaluateFolds |> Seq.averageBy fst
// average on validation part
evaluateFolds |> Seq.averageBy snd


// Comparing trees & forests

let forestFeatures = [
    "Sex", fun (p:Passenger) -> p.Sex |> Some
    "Class", fun p -> p.Pclass |> string |> Some
    "Age", fun p -> if p.Age < 7.0 then Some("Younger") else Some("Older")
    "Port", fun p -> if p.Embarked = "" then None else Some(p.Embarked) ]


let forestResults () =

    let accuracy predictor (sample:Passenger seq) = 
        sample 
        |> Seq.averageBy (fun p -> 
            if p.Survived = predictor p then 1.0 else 0.0)

    [for (training,validation) in folds ->   
         
        let forest = growForest 1000 training label forestFeatures

        let accuracyTraining = accuracy forest training
        let accuracyValidation = accuracy forest validation

        printfn "Training: %.3f, Validation: %.3f" accuracyTraining accuracyValidation 
        accuracyTraining,accuracyValidation ]

// run the forest on 10 folds
// results will vary from run to run
let forestEvaluation = forestResults ()

// average on training part    
forestEvaluation |> Seq.averageBy fst
// average on validation part
forestEvaluation |> Seq.averageBy snd