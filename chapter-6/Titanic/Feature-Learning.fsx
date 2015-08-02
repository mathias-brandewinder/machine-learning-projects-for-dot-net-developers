#I "./packages"
#r @"FSharp.Data.2.2.2/lib/net40/FSharp.Data.dll"
open FSharp.Data

type Titanic = CsvProvider<"titanic.csv">
type Passenger = Titanic.Row

let dataset = Titanic.GetSample ()

// entropy of sample
let entropy label data = 
    let size = data |> Seq.length
    data 
    |> Seq.countBy label 
    |> Seq.map (fun (_,count) -> float count / float size)
    |> Seq.sumBy (fun f -> if f > 0. then - f * log f else 0.)

// chart examples

let ex1 = [1;1;1;2;2;2;3;3;3] |> entropy id
let ex2 = [1;1;1;1;1;1;1;1;1] |> entropy id
let ex3 = [1;1;1;1;1;2;2;3;3] |> entropy id
let ex4 = [1;1;1;1;1;1;1;2;3] |> entropy id

 
// average entropy if break by feature

let hasData feature = feature >> Option.isSome

let splitEntropy extractLabel extractFeature data = 
    // observations with no missing values
    // for the selected feature
    let dataWithValues = 
        data 
        |> Seq.filter (extractFeature |> hasData)
    let size = dataWithValues |> Seq.length
    dataWithValues 
    |> Seq.groupBy extractFeature
    |> Seq.sumBy (fun (_,group) ->
        let groupSize = group |> Seq.length
        let probaGroup = float groupSize / float size
        let groupEntropy = group |> entropy extractLabel
        probaGroup * groupEntropy) 


// compare features on entire set

let survived (p:Passenger) = p.Survived

let sex (p:Passenger) = Some(p.Sex) 
let pclass (p:Passenger) = Some(p.Pclass)
let port (p:Passenger) = 
    if p.Embarked = ""
    then None
    else Some(p.Embarked)
let age (p:Passenger) = 
    if p.Age < 12.0 
    then Some("Younger") 
    else Some("Older")
    
printfn "Comparison: most informative feature"
let h = dataset.Rows |> entropy survived
printfn "Base entropy %.3f" h

dataset.Rows |> splitEntropy survived sex |> printfn "  Sex: %.3f"
dataset.Rows |> splitEntropy survived pclass |> printfn "  Class: %.3f"
dataset.Rows |> splitEntropy survived port |> printfn "  Port: %.3f"
dataset.Rows |> splitEntropy survived age |> printfn "  Age: %.3f"

// procedure can then be repeated...

let bySex = dataset.Rows |> Seq.groupBy sex

for (groupName, group) in bySex do
    printfn "Group: %s" groupName.Value    
    let h = group |> entropy survived
    printfn "Base entropy %.3f" h

    group |> splitEntropy survived sex |> printfn "  Sex: %.3f"
    group |> splitEntropy survived pclass |> printfn "  Class: %.3f"
    group |> splitEntropy survived port |> printfn "  Port: %.3f"
    group |> splitEntropy survived age |> printfn "  Age: %.3f"


// analyzing a list of features

let test () =
    
    let survived (p:Passenger) = p.Survived

    let sex (p:Passenger) = Some(p.Sex) 
    let pclass (p:Passenger) = Some(p.Pclass |> string)

    // features now have a consistent type, 
    // so we can put them all in a list.
    let features = 
        [   "Sex", sex
            "Class", pclass
        ]

    features 
    |> List.iter (fun (name, feat) -> 
        dataset.Rows 
        |> splitEntropy survived feat |> printfn "%s: %.3f" name)

test ()


// using entropy to partition continuous features

let bestAge () =
    
    let ages = dataset.Rows |> Seq.map (fun p -> p.Age) |> Seq.distinct
    let best =
        ages 
        |> Seq.minBy (fun a ->        
            let age (p:Passenger) = 
                if p.Age < a then Some("Kid") else Some("Adult")
            dataset.Rows |> splitEntropy survived age)
    printfn "Best age split"
    printfn "Age: %.3f" best

bestAge ()   