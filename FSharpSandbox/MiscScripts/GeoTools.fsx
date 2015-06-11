#load @"c:\Users\Tomas\Scripts\NuGet\LoadPackages.fsx"

let nu = 0.3 // [-]
let gamma = 20.0 // kN/m3
let e0 = 0.5
let constant = log 10.0 // Due to the change of log to ln.

let getKd Cr = constant * (1.0 + nu) * (1.0 - 2.0 * nu) / (1.0 - nu) * gamma * (1.0 + e0) / Cr

(*
Zemina Modul přetvárnosti           Eoed [MPa]
štěrkovitá                          60--600
písčitá středně ulehlá až ulehlá    7--130
soudržná                            2--30
*)
// F6 podle konzistence od 1.5 Mpa do 28 MPa

let minE = 2.0e3  // kPa
let maxE = 30.0e3 // kPa
let minCr = 0.2 / 10.0
let maxCr = 0.5 / 5.0

let maxH = 50.0

open FSharp.Charting
FSharp.Charting.Chart.Combine(
  [
    FSharp.Charting.Chart.Line([(0.0, minE); (maxH, minE)], Name="minimal E")
    FSharp.Charting.Chart.Line([(0.0, maxE); (maxH, maxE)], Name="maximal E")
    FSharp.Charting.Chart.Line([(0.0, 0.0); (maxH, maxH * getKd maxCr)], Name="maximal Cr")
    FSharp.Charting.Chart.Line([(0.0, 0.0); (maxH, maxH * getKd minCr)], Name="minimal Cr")
  ]).WithLegend(Enabled=true)

