(*
Crash course: F# Set
*)

let set1 = set [1;2;3]
let set2 = set [1;3;5]

// Produces set [1;3]
let intersection = Set.intersect set1 set2
// Produces set [1;2;3;5]
let union = Set.union set1 set2
// Produces set [2]
let diff1 = Set.difference set1 set2
// Produces set [5]
let diff2 = Set.difference set2 set1

// Sets are immutable:
// this produces a new set
let set3 = Set.add 4 set1

(*
Crash course: F# Sequence
*)

// Generate a sequence of 1, 2, .. 10
let seq1 = seq { for x in 1 .. 10 -> x }

// Double each element, with a side-effect:
// whenever a calculation is happening,
// print a message out.
let seq2 =
    seq1 
    |> Seq.map (fun x -> 
        printfn "mapping %i" x
        2 * x) 

// Seq.length needs to evaluate seq2;
// Note that only 3 terms are evaluated.
let seq3 = 
    seq2 
    |> Seq.take 3 
    |> Seq.length

// By contrast, Array eagerly evaluates
// everything, regardless of how much
// is actually needed.
let arr1 = [| for x in 1 .. 10 -> x |]
let arr2 =
    arr1 
    |> Array.map (fun x -> 
        printfn "mapping %i" x 
        2 * x) 