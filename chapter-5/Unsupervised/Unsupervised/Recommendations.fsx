(* Simple recommendation *)

open System
open System.IO

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
        |> Array.filter (fun row -> (row |> Array.sum) > 0.)
    
    headers,observations

// User based

let scale (row:float[]) =
    let min = row |> Array.min 
    let max = row |> Array.max
    if min = max 
    then row
    else 
        row |> Array.map (fun x -> (x - min) / (max - min))

let test  = observations.[..99]  |> Array.map scale
let train = observations.[100..] |> Array.map scale

let distance (row1:float[]) (row2:float[]) =
    (row1,row2)
    ||> Array.map2 (fun x y -> pown (x - y) 2)
    |> Array.sum

let similarity (row1:float[]) (row2:float[]) =
    1. / (1. + distance row1 row2)

let cosine (row1:float[]) (row2:float[]) =
    let top = (row1,row2) ||> Array.map2 (fun x y -> x * y) |> Array.sum
    let l2 xs = xs |> Array.sumBy (fun x -> x * x) |> sqrt
    let bottom = l2 row1 * l2 row2
    if bottom = 0. 
    then 0.0 
    else 0.5 + 0.5 * top / bottom

let split (row:float[]) =
    row.[..19],row.[20..]

let weights (values:float[]) =
    let total = values |> Array.sum
    values 
    |> Array.map (fun x -> x / total)

let predict (row:float[]) =
    let known,unknown = row |> split 
    let similarities = 
        train
        |> Array.map (fun example ->          
            let common, _ = example |> split
            similarity known common)
        |> weights
    [| for i in 20 .. 29 -> 
        let column = train |> Array.map (fun x -> x.[i])
        let prediction = 
            (similarities,column) 
            ||> Array.map2 (fun s v -> s * v) 
            |> Array.sum
        prediction |]

// making a prediction
let targetTags = headers.[20..]
predict test.[0] |> Array.zip targetTags

let average = 
    [|
        for i in 20 .. 29 ->
            train |> Array.map (fun row -> row.[i]) |> Array.average
    |]

let check i =
    let _,real = test.[i] |> split
    printfn ""
    real |> Seq.iter (printf "%.2f ")
    printfn ""
    predict (test.[i]) |> Seq.iter (printf "%.2f ")
    printfn ""
    average |> Seq.iter (printf "%.2f ")


// measuring quality

// 1. Cases where recommendation matches

let validation =
    test
    |> Array.map (fun obs ->
        let actual = obs |> split |> snd
        let predicted = obs |> predict
        let recommended, observed = 
            Array.zip predicted actual
            |> Array.maxBy fst
        if observed > 0. then 1. else 0.)
    |> Array.average
    |> printfn "Correct calls: %f" 

let averages =  [| 
    for i in 20 .. 29 -> 
        train |> Array.averageBy(fun row -> row.[i]) |]

let baseline =
    test
    |> Array.map (fun obs ->
        let actual = obs |> split |> snd
        let predicted = averages
        let recommended, observed = 
            Array.zip predicted actual
            |> Array.maxBy fst
        if observed > 0. then 1. else 0.)
    |> Array.average
    |> printfn "Correct calls: %f" 


let tags = headers.[20..]

let recommend i =
    let _,real = test.[i] |> split
    //Array.zip tags real |> Array.iter (fun (t,s) -> printfn "%s %.2f" t s)
    predict (test.[i]) |> Array.zip tags |> Array.maxBy snd
test |> Array.mapi (fun i x -> i,recommend i)

test
|> Array.mapi (fun i obs -> 
    let predicted = predict (test.[i])
    let _,real = test.[i] |> split
    (predicted,real)
    ||> Array.zip 
    |> Array.maxBy fst 
    |> (fun (_,r) -> if r > 0. then 1. else 0.))
|> Array.average

test
|> Array.mapi (fun i obs -> 
    let predicted = average
    let _,real = test.[i] |> split
    (predicted,real)
    ||> Array.zip 
    |> Array.maxBy fst 
    |> (fun (_,r) -> if r > 0. then 1. else 0.))
|> Array.average


// item based? with cosine?
