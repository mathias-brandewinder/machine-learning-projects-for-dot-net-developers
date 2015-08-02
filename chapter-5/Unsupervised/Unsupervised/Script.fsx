(*
Loading the dataset
*)

open System
open System.IO

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
The dataset: basic characteristics
*)

printfn "%16s %8s %8s %8s" "Tag Name" "Avg" "Min" "Max"

headers
|> Array.iteri (fun i name ->
    let col = observations |> Array.map (fun obs -> obs.[i])
    let avg = col |> Array.average
    let min = col |> Array.min
    let max = col |> Array.max
    printfn "%16s %8.1f %8.1f %8.1f" name avg min max)


// plotting average tag usage

#r @"../packages/FSharp.Charting/lib/net40/FSharp.Charting.dll"
#load @"../packages/FSharp.Charting/FSharp.Charting.fsx"
open FSharp.Charting

let labels = ChartTypes.LabelStyle(Interval=0.25)

headers
|> Seq.mapi (fun i name -> 
    name, 
    observations 
    |> Seq.averageBy (fun obs -> obs.[i]))
|> Chart.Bar
|> fun chart -> chart.WithXAxis(LabelStyle=labels)

(*
K-means clustering
Clustering: first pass.
No transforms.
*)

#load "KMeans.fs"
open Unsupervised.KMeans

type Observation = float []

let features = headers.Length

let distance (obs1:Observation) (obs2:Observation) =
    (obs1, obs2) 
    ||> Seq.map2 (fun u1 u2 -> pown (u1 - u2) 2)
    |> Seq.sum
    |> sqrt

let centroidOf (cluster:Observation seq) =
    Array.init features (fun f ->
        cluster 
        |> Seq.averageBy (fun user -> user.[f]))

let observations1 = 
    observations 
    |> Array.map (Array.map float) 
    |> Array.filter (fun x -> Array.sum x > 0.)

let (clusters1, classifier1) = 
    let clustering = clusterize distance centroidOf
    let k = 5
    clustering observations1 k

// cluster profiles
   
clusters1
|> Seq.iter (fun (id,profile) ->
    printfn "CLUSTER %i" id
    profile 
    |> Array.iteri (fun i value -> printfn "%16s %.1f" headers.[i] value))

Chart.Combine [
    for (id,profile) in clusters1 -> 
        profile 
        |> Seq.mapi (fun i value -> headers.[i], value)
        |> Chart.Bar
    ]
|> fun chart -> chart.WithXAxis(LabelStyle=labels)

// cluster size

observations1
|> Seq.countBy (fun obs -> classifier1 obs)
|> Seq.iter (fun (clusterID, count) -> 
    printfn "Cluster %i: %i elements" clusterID count)

// plotting clusters against features
let plotClusters (dataset:float[] seq) classifier feat1 feat2 =
    dataset 
    |> Seq.map (fun obs -> (obs.[feat1], obs.[feat2]), classifier obs)
    |> Seq.groupBy (fun (_,cluster) -> cluster)
    |> Seq.map (fun (_, group) -> 
        [ for (coords, _) in group -> coords ] |> Chart.Point)
    |> Seq.toList
    |> Chart.Combine
    |> Chart.WithTitle (sprintf "%s vs. %s" headers.[feat1] headers.[feat2])

(*
Normalized observations
Row normalization: largest tag = 100%
i.e. small distance = users that have
same interest profile, not necessarily
same level of activity on site.
*)
    
// sorted features
headers
|> Seq.mapi (fun i name ->
    let col = observations |> Seq.map (fun obs -> float obs.[i])
    name, col |> Seq.average)
|> Seq.sortBy snd
|> Seq.iter (printfn "%A")



let rowNormalizer (obs:Observation) : Observation = 
    let max = obs |> Seq.max
    obs |> Array.map (fun tagUse -> tagUse / max)

let observations2 = 
    observations 
    |> Array.filter (fun x -> Array.sum x > 0.)
    |> Array.map rowNormalizer

let (clusters2, classifier2) = 
    let clustering = clusterize distance centroidOf
    let k = 5    
    clustering observations2 k
    
observations2
|> Seq.countBy (fun obs -> classifier2 obs)
|> Seq.iter (fun (clusterID, count) -> 
    printfn "Cluster %i: %i elements" clusterID count)

Chart.Combine [
    for (id,profile) in clusters2 -> 
        profile 
        |> Seq.mapi (fun i value -> headers.[i], value)
        |> Chart.Column
    ]
|> fun chart -> chart.WithXAxis(LabelStyle=labels)

clusters2
|> Seq.iter (fun (id,profile) ->
    printfn "CLUSTER %i" id
    profile 
    |> Array.iteri (fun i value -> printfn "%16s %.1f" headers.[i] value))

(*
Figuring out a reasonable value for
k, the number of clusters
*)

let ruleOfThumb (n:int) = sqrt (float n / 2.)
let k_ruleOfThumb = ruleOfThumb (observations2.Length) // 24 to 25 clusters!

// Akaike information criterion
// see http://nlp.stanford.edu/IR-book/html/htmledition/k-means-1.html

let squareError (obs1:Observation) (obs2:Observation) =
    (obs1,obs2) 
    ||> Seq.zip 
    |> Seq.sumBy (fun (x1,x2) -> pown (x1-x2) 2)

let RSS (dataset:Observation[]) centroids =
    dataset 
    |> Seq.sumBy (fun obs -> 
        centroids 
        |> Seq.map (squareError obs)
        |> Seq.min)

let AIC (dataset:Observation[]) centroids = 
    let k = centroids |> Seq.length
    let m = dataset.[0] |> Seq.length
    RSS dataset centroids + float (2 * m * k)

// computing AIC over every k in 1 .. 25
// running 10 clusters each to avoid "flukes"

[1..25]
|> Seq.map (fun k ->
    let value = 
        [ for _ in 1 .. 10 ->
            let (clusters, classifier) = 
                let clustering = clusterize distance centroidOf
                clustering observations2 k
            AIC observations2 (clusters |> Seq.map snd) ]
        |> List.average
    k, value)
|> Chart.Line


// Final clusters

let (bestClusters, bestClassifier) =
    let clustering = clusterize distance centroidOf
    let k = 10    
    seq { 
        for _ in 1 .. 20 ->
            clustering observations2 k
    } 
    |> Seq.minBy (fun (cs,f) -> 
        RSS observations2 (cs |> Seq.map snd))
    
observations2
|> Seq.countBy (fun obs -> bestClassifier obs)
|> Seq.iter (fun (clusterID, count) -> 
    printfn "Cluster %i: %i elements" clusterID count)

Chart.Combine [
    for (id,profile) in bestClusters -> 
        profile 
        |> Seq.mapi (fun i value -> headers.[i], value)
        |> Chart.Column
    ]
|> fun chart -> chart.WithXAxis(LabelStyle=labels)

bestClusters
|> Seq.iter (fun (id,profile) ->
    printfn "CLUSTER %i" id
    profile 
    |> Array.iteri (fun i value -> 
        if value > 0.2 then printfn "%16s %.1f" headers.[i] value))