#load @"c:\Users\Tomas\Scripts\NuGet\LoadPackages.fsx"

// Normal probability distribution function (pdf)
let pdfNorm x mu sigma = 1.0 / sqrt(2.0 * System.Math.PI) / sigma * System.Math.Exp(-2.0 * (x - mu)**2.0 / 2.0 / sigma**2.0)

// Normal pdf viewed as function of x
let f x = pdfNorm x 0.0 1.0

let x = 

let getPoints f x =
  let y = x |> List.map f
  List.zip x y 

let points = [-2.0..0.1..2.0] |> getPoints f |>  FSharp.Charting.Chart.Line
FSharp.Charting.Chart.Line points

let fsig sigma = pdfNorm 0.0 0.0 sigma
let x2 = [0.01..0.01..2.0]
let points2 = getPoints fsig x2
FSharp.Charting.Chart.Line points2
