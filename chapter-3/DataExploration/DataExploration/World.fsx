#I @"../../../packages"
#r @"FSharp.Data/lib/net40/FSharp.Data.dll"

open FSharp.Data

let wb = WorldBankData.GetDataContext ()
let countries = wb.Countries

let pop2000 = [ for c in countries -> c.Indicators.``Population, total``.[2000]]
let pop2010 = [ for c in countries -> c.Indicators.``Population, total``.[2010]]

#r @"R.NET.Community/lib/net40/RDotNet.dll"
#r @"RProvider/lib/net40/RProvider.Runtime.dll"
#r @"RProvider/lib/net40/RProvider.dll"

open RProvider
open RProvider.``base``
open RProvider.graphics

// Retrieve an (F#) list of country surfaces
let surface = [ for c in countries -> c.Indicators.``Surface area (sq. km)``.[2010]]
// Produce summary statistics
R.summary(surface) |> R.print
// Plot the corresponding histogram
R.hist(surface)
// Plot the transform
R.hist(surface |> R.log)
R.hist(surface |> List.map log)

// Plot population against surface
R.plot(surface, pop2010)
R.plot(surface |> R.log, pop2010 |> R.log)

// create data frame, the hard way

let pollution = [ for c in countries -> c.Indicators.``CO2 emissions (kt)``.[2000]]
let education = [ for c in countries -> c.Indicators.``School enrollment, secondary (% gross)``.[2000]]

let rdf =
    [ "Pop2000", box pop2000
      "Pop2010", box pop2010
      "Surface", box surface
      "Pollution", box pollution
      "Education", box education ]
    |> namedParams
    |> R.data_frame

// Scatterplot of all features
rdf |> R.plot

// Summary of all features
rdf |> R.summary |> R.print

(*
Using Deedle dataframe and 
feeding it into R with the R plugin
*)

#r @"Deedle/lib/net40/Deedle.dll"
open Deedle

let series1 = series [ "Alpha", 1.; "Bravo", 2.; "Delta", 4. ]
let series2 = series [ "Bravo", 20.; "Charlie", 30.; "Delta", 40. ]
let toyFrame = frame [ "First", series1; "Second", series2 ]

series1 |> Stats.sum
toyFrame |> Stats.mean
toyFrame?Second |> Stats.mean

toyFrame?New <- toyFrame?First + toyFrame?Second
toyFrame |> Stats.mean

#r @"Deedle.RPlugin/lib/net40/Deedle.RProvider.Plugin.dll"
open Deedle.RPlugin

let population2000 = 
    series [ for c in countries -> c.Code, c.Indicators.``Population, total``.[2000]]
let population2010 = 
    series [ for c in countries -> c.Code, c.Indicators.``Population, total``.[2010]]
let surfaceArea = 
    series [ for c in countries -> c.Code, c.Indicators.``Surface area (sq. km)``.[2010]]

let ddf = 
    frame [ 
        "Pop2000", population2000
        "Pop2010", population2010
        "Surface", surfaceArea ]

R.plot ddf