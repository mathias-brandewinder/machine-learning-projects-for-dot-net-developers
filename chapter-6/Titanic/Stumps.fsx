#r @"../../packages/FSharp.Data/lib/net40/FSharp.Data.dll"
open FSharp.Data

type Titanic = CsvProvider<"titanic.csv">
type Passenger = Titanic.Row

let dataset = Titanic.GetSample ()

(*
Probability of surviving
*)

dataset.Headers.Value 
|> Seq.iter (printfn "%A") 

dataset.Rows 
|> Seq.countBy (fun passenger -> passenger.Survived)
|> Seq.iter (printfn "%A")

dataset.Rows 
|> Seq.averageBy (fun passenger ->
    if passenger.Survived then 1.0 else 0.0)
|> printfn "Chances of survival: %.3f"


(*
Possible predictors: lead towards 
entropy, and bring up missing values.
*)

let survivalRate (passengers:Passenger seq) =
    let total = passengers |> Seq.length
    let survivors = 
        passengers 
        |> Seq.filter (fun p -> p.Survived)
        |> Seq.length
    100.0 * (float survivors / float total)

let bySex = 
    dataset.Rows 
    |> Seq.groupBy(fun p -> p.Sex)

bySex 
|> Seq.iter (fun (s,g) ->
    printfn "Sex %A: %f" s (survivalRate g))

let byClass = 
    dataset.Rows
    |> Seq.groupBy (fun p -> p.Pclass)

byClass
|> Seq.iter (fun (s,g) ->
    printfn "Class %A: %f" s (survivalRate g))


(*
Learning basic decision stumps
*)

let mostFrequentLabelIn group =
    group 
    |> Seq.countBy snd 
    |> Seq.maxBy snd 
    |> fst

let learn sample extractFeature extractLabel = 
    // group together observations that have the
    // same value for the selected feature, and
    // find the most frequent label by group.
    let groups =
        sample
        |> Seq.map (fun obs -> extractFeature obs, extractLabel obs)
        |> Seq.groupBy fst
        |> Seq.map (fun (feat,group) -> feat, mostFrequentLabelIn group)
    // for an observation, find the group with
    // matching feature value, and predict the
    // most frequent label for that group.
    let classifier obs =
        let featureValue = extractFeature obs
        groups
        |> Seq.find (fun (f,_) -> f = featureValue)
        |> snd 
    classifier


let survived (p:Passenger) = p.Survived
let sex (p:Passenger) = p.Sex

let sexClassifier = survived |> learn (dataset.Rows) sex

// checking predictions on a couple observations
dataset.Rows
|> Seq.take 10
|> Seq.iter (fun p ->
    printfn "Real: %A, Pred: %A" p.Survived (sexClassifier p))

printfn "Stump: classify based on passenger sex."
dataset.Rows
|> Seq.averageBy (fun p -> 
    if p.Survived = sexClassifier p then 1.0 else 0.0)

printfn "Stump: classify based on passenger class."
let classClassifier = survived |> learn (dataset.Rows) (fun p -> p.Pclass)

dataset.Rows
|> Seq.averageBy (fun p -> 
    if p.Survived = classClassifier p then 1.0 else 0.0)


(*
Continuous / numeric features
*)

let survivalByPricePaid =
    dataset.Rows
    |> Seq.groupBy (fun p -> p.Fare)
    |> Seq.sortBy fst
    |> Seq.iter (fun (price,passengers) ->
        printfn "%6.2F: %6.2f" price (survivalRate passengers))

// how many cases are there?
dataset.Rows
|> Seq.map (fun p -> p.Fare)
|> Seq.distinct
|> Seq.length

// transforming fare into discrete "bins"

let averageFare = 
    dataset.Rows 
    |> Seq.averageBy (fun p -> p.Fare)

let fareLevel (p:Passenger) = 
    if p.Fare < averageFare 
    then "Cheap" 
    else "Expensive"

printfn "Stump: classify based on fare level."
let fareClassifier = survived |> learn (dataset.Rows) fareLevel

dataset.Rows
|> Seq.averageBy (fun p -> 
    if p.Survived = fareClassifier p then 1.0 else 0.0)



(*
Missing values
*)

let survivalByPortOfOrigin =
    dataset.Rows
    |> Seq.groupBy (fun p -> p.Embarked)
    |> Seq.iter (fun (port,passengers) ->
        printfn "%1s: %6.2f" port (survivalRate passengers))

dataset.Rows |> Seq.countBy (fun p -> p.Embarked)


// Using Option<'a> to represent missing values

let hasData extractFeature = extractFeature >> Option.isSome

let betterLearn sample extractFeature extractLabel = 
    let branches =
        sample 
        |> Seq.filter (extractFeature |> hasData)
        |> Seq.map (fun obs -> extractFeature obs |> Option.get, extractLabel obs)
        |> Seq.groupBy fst
        |> Seq.map (fun (feat,group) -> feat, mostFrequentLabelIn group)
        |> Map.ofSeq
    let labelForMissingValues = 
        sample 
        |> Seq.countBy extractLabel 
        |> Seq.maxBy snd 
        |> fst
    let classifier obs =
        let featureValue = extractFeature obs
        match featureValue with
        | None -> labelForMissingValues
        | Some(value) ->
            match (branches.TryFind value) with
            | None -> labelForMissingValues
            | Some(predictedLabel) -> predictedLabel
    classifier

let port (p:Passenger) =
    if p.Embarked = "" then None
    else Some(p.Embarked)

let updatedClassifier = survived |> betterLearn (dataset.Rows) port

dataset.Rows
|> Seq.averageBy (fun p -> 
    if p.Survived = updatedClassifier p then 1.0 else 0.0)