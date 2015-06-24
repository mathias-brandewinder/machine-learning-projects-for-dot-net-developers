namespace Titanic

module Tree =

    open System

    type Feature<'a> = 'a -> string Option
    type NamedFeature<'a> = string * Feature<'a>
    type Label<'a,'b when 'b:equality> = 'a -> 'b

    type Tree<'a,'b when 'b:equality> = 
        | Answer of 'b
        | Stump of NamedFeature<'a> * string * Map<string,Tree<'a,'b>>
    
    let rec decide tree observation = 
        match tree with
        | Answer(labelValue) -> labelValue
        | Stump((featureName,feature),valueWhenMissing,branches) ->
            let featureValue = feature observation
            let usedValue =
                match featureValue with
                | None -> valueWhenMissing
                | Some(value) -> 
                    match (branches.TryFind value) with
                    | None -> valueWhenMissing
                    | Some(_) -> value
            let nextLevelTree = branches.[usedValue]
            decide nextLevelTree observation

    let entropy label data = 
        let size = data |> Seq.length
        data 
        |> Seq.countBy label 
        |> Seq.map (fun (_,count) -> float count / float size)
        |> Seq.sumBy (fun f -> if f > 0. then - f * log f else 0.)

    let splitEntropy label feature data = 
        let knownData = 
            data 
            |> Seq.filter (fun obs -> feature obs |> Option.isSome)
        let size = knownData |> Seq.length
        knownData 
        |> Seq.groupBy feature
        |> Seq.sumBy (fun (_,group) ->
            let groupSize = group |> Seq.length
            let groupEntropy = group |> entropy label
            (float groupSize / float size) * groupEntropy) 

    let mostFrequentBy f sample = 
        sample 
        |> Seq.map f 
        |> Seq.countBy id 
        |> Seq.maxBy snd 
        |> fst

    let rec growTree sample label features = 

        if (Map.isEmpty features)
        // we have no feature left to split on:
        // our prediction is the most frequent
        // label in the dataset
        then sample |> mostFrequentBy label |> Answer
        else
            // from the named features we have available,
            // identify the one with largest entropy gain.
            let (bestName,bestFeature) = 
                features
                |> Seq.minBy (fun kv -> 
                    splitEntropy label kv.Value sample)
                |> (fun kv -> kv.Key, kv.Value)
            // create a group for each of the values the
            // feature takes, eliminating the cases where
            // the value is missing.
            let branches = 
                sample
                |> Seq.groupBy bestFeature
                |> Seq.filter (fun (value,group) -> value.IsSome)
                |> Seq.map (fun (value,group) -> value.Value,group)
            // find the most frequent value for the feature;
            // we'll use it as a default replacement for missing values.
            let defaultValue = 
                branches 
                |> Seq.maxBy (fun (value,group) -> 
                    group |> Seq.length)
                |> fst
            // remove the feature we selected from the list of
            // features we can use at the next tree level
            let remainingFeatures = features |> Map.remove bestName
            // ... and repeat the operation for each branch,
            // building one more level of depth in the tree
            let nextLevel = 
                branches 
                |> Seq.map (fun (value,group) -> 
                    value, growTree group label remainingFeatures)
                |> Map.ofSeq

            Stump((bestName,bestFeature),defaultValue,nextLevel)

    let rec display depth tree =
        let padding = String.replicate (2 * depth) " "
        match tree with
        | Answer(label) -> printfn " -> %A" label
        | Stump((name,_),_,branches) ->
            printfn ""
            branches
            |> Seq.iter (fun kv -> 
                printf "%s ? %s : %s" padding name kv.Key
                display (depth + 1) kv.Value)

    let entropyGainFilter sample label feature = 
        splitEntropy label feature sample - entropy label sample < 0.

    let leafSizeFilter minSize sample label feature =
        sample 
        |> Seq.map feature 
        |> Seq.choose id
        |> Seq.countBy id
        |> Seq.forall (fun (_,groupSize) -> groupSize > minSize)

    let rec growTree2 filters sample label features = 

        let features =
            features
            |> Map.filter (fun name feature -> 
                filters |> Seq.forall (fun filter -> filter sample label feature)) 

        if (Map.isEmpty features)
        then sample |> mostFrequentBy label |> Answer
        else
            let (bestName,bestFeature) = 
                features
                |> Seq.minBy (fun kv -> 
                    splitEntropy label kv.Value sample)
                |> (fun kv -> kv.Key, kv.Value)
            let branches = 
                sample
                |> Seq.groupBy bestFeature
                |> Seq.filter (fun (value,group) -> value.IsSome)
                |> Seq.map (fun (value,group) -> value.Value,group)
            let defaultValue = 
                branches 
                |> Seq.maxBy (fun (value,group) -> 
                    group |> Seq.length)
                |> fst
            let remainingFeatures = features |> Map.remove bestName
            let nextLevel = 
                branches 
                |> Seq.map (fun (value,group) -> 
                    value, growTree2 filters group label remainingFeatures)
                |> Map.ofSeq

            Stump((bestName,bestFeature),defaultValue,nextLevel)

// forest code

    let pickNoRepeat (rng:Random) proportion original =
     
        let size = original |> Seq.length
        let sampleSize = proportion * float size |> int
                   
        let init = ([],size)
        original 
        |> Seq.fold (fun (sampled,remaining) item ->
            let picked = List.length sampled
            let p = float (sampleSize - picked) / float remaining
            if (rng.NextDouble () <= p)
            then (item::sampled,remaining-1)
            else (sampled,remaining-1)) init
        |> fst

    let pickRepeat (rng:Random) proportion original =     
        let size = original |> Array.length
        let sampleSize = proportion * float size |> int
        Array.init sampleSize (fun _ -> original.[rng.Next(size)])  

    let predict forest observation =
        forest 
        |> Seq.map (fun tree -> decide tree observation)
        |> Seq.countBy id
        |> Seq.maxBy snd
        |> fst

    let growForest size sample label features =

        let rng = Random ()

        let propFeatures =
            let total = features |> Seq.length |> float
            sqrt total / total 

        let featSample () = pickNoRepeat rng propFeatures features
        let popSample () = pickRepeat rng 1.0 sample
        let filters = [ leafSizeFilter 10; entropyGainFilter ]
        
        let forest = [ 
            for _ in 1 .. size -> 
                let sample = popSample ()
                let features = featSample () |> Map.ofList
                growTree2 filters sample label features ]

        let predictor = predict forest
        predictor