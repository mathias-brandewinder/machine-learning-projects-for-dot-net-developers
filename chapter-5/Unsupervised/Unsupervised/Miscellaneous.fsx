(*
Create illustration samples:
what types of patterns can exist in data?
what problems can arise?
*)

open System

let seed = 1234567890
let rng = Random (seed)
let dbl () = rng.NextDouble ()

#r @"../packages/FSharp.Charting/lib/net40/FSharp.Charting.dll"
#load @"../packages/FSharp.Charting/FSharp.Charting.fsx"

open FSharp.Charting

let unorganized = Seq.init 1000 (fun _ -> dbl(), dbl())
(Chart.Point (unorganized)).WithXAxis(Min=0.,Max=1.)

// clustered data

let discusGenerator center radius =
    let cx,cy = center
    let generator () =
        let angle = 2. * dbl () * Math.PI
        let dist = - log (1. - dbl ()) * radius
        let x = dist * cos angle
        let y =  dist * sin angle
        cx + x, cy + y
    generator

let clustered = seq {

    for _ in 1 .. 3 do

        let radius =  1. + 1. * dbl ()
        let center = 10. * dbl(), 10. * dbl()
        let generator = discusGenerator center radius

        let sample = 
            Seq.init 1000 (fun _ -> generator ())
        yield! sample }

(Chart.Point (clustered))
    .WithXAxis(Max=20.,Min= -10.)
    .WithYAxis(Max=20.,Min= -10.)

// noisy linear features

let feature x = 3. * x + 10.

let noisyCombo = Seq.init 1000 (fun _ ->
    let x = dbl ()
    let e = dbl () - 0.5
    let error = pown (3. * e) 3 
    x, feature (x + 0.5 * error))

(Chart.Point (noisyCombo))
    .WithXAxis(Max=1.,Min= 0.)
    .WithYAxis(Min=9.,Max=13.)

// "hard to describe" patterns

let ringGenerator center radius width =
    let cx,cy = center
    let generator () =
        let angle = 2. * dbl () * Math.PI
        let d = dbl ()
        let dist = radius + (width * d * d)
        let x = dist * cos angle
        let y =  dist * sin angle
        cx + x, cy + y      
    generator      

let rings = seq {

    for _ in 1 .. 3 do

        let radius =  3.
        let width = 1.
        let center = 10. * dbl(), 10. * dbl()

        let generator = ringGenerator center radius width

        let sample = 
            Seq.init 1000 (fun _ -> generator ())
        yield! sample }

(Chart.Point (rings))
    .WithXAxis(Min=0.,Max=10.)
    .WithYAxis(Min=0.,Max=10.)

(*
Problematic clusters
*)

let threeClusters = seq {
    for center in [ (16.,5.); (3.,2.); (11.,18.) ] do

        let radius =  1. + 1. * dbl ()
        let generator = discusGenerator center radius

        let sample = Seq.init 1000 (fun _ -> generator ())
        yield! sample }

(Chart.Point (threeClusters))
    .WithXAxis(Max=20.,Min= 0.)
    .WithYAxis(Max=20.,Min= 0.)
    .WithMarkers(Size=2)


let howManyClusters = seq {
    for (x,y,s) in [ (14.,5.,500); (16.,3.,500); (18.,5.,500); (3.,2.,2000); (3.,7.,200); (10.,18.,1000); (12.,16.,1000) ] do

        let radius =  1. + 1. * dbl ()
        let generator = discusGenerator (x,y) radius

        let sample = Seq.init s (fun _ -> generator ())
        yield! sample }

(Chart.Point (howManyClusters))
    .WithXAxis(Max=20.,Min= 0.)
    .WithYAxis(Max=20.,Min= 0.)
    .WithMarkers(Size=2)

let ellipseGenerator center X Y =
    let cx,cy = center
    let generator () =
        let angle = 2. * dbl () * Math.PI
        let dist = - log (1. - dbl ())
        let x = dist * X * cos angle
        let y =  dist * Y * sin angle
        cx + x, cy + y      
    generator      

let badMetric = seq {
    for center in [ (-2.,0.); (2.,0.); ] do

        let radius =  1. + 1. * dbl ()
        let generator = ellipseGenerator center 0.5 10.

        let sample = Seq.init 1000 (fun _ -> generator ())
        yield! sample }

(Chart.Point (badMetric))
    .WithXAxis(Max=20.,Min= -20.)
    .WithYAxis(Max=20.,Min= -20.)
    .WithMarkers(Size=2)

let rescaled = 
    let xmin = badMetric |> Seq.map fst |> Seq.min
    let xmax = badMetric |> Seq.map fst |> Seq.max
    let ymin = badMetric |> Seq.map snd |> Seq.min
    let ymax = badMetric |> Seq.map snd |> Seq.max
    badMetric
    |> Seq.map (fun (x,y) -> 
        (x - xmin) / (xmax - xmin), 
        (y-ymin) / (ymax - ymin))

(Chart.Point (rescaled))
    .WithXAxis(Max=1.,Min= 0.)
    .WithYAxis(Max=1.,Min= 0.)
    .WithMarkers(Size=2)
