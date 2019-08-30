#load @"c:\Users\Tomas\Scripts\Paket\References.fsx"
#load @"AdjustAccelerogram.fsx"

open System
open System.Runtime.InteropServices
open System.IO
open System.Numerics // System.Numerics.Complex
open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra // Vector<>, Matrix<>
open FSharp.Charting // Chart

// SI units are used in this script (m, s, kg, N, Pa).

// This script adjusts the accelerogram used in NMM2019 paper.
// The tools for ajusting accelerograms are in AdjustAccelerogram. This is only operational file.

// TASKS:
// - Read history_kumar.csv (original)
// - Determine dt
// - Integrate original
// - Plot original
// - Adjust
// - Integrate adjusted
// - Plot adjusted
// - Save adjusted to csv

let dir = __SOURCE_DIRECTORY__
let path = dir + @"\history_kumar.csv"
let path_adj = dir + @"\history_adjusted.csv"
let (beta, gamma) = 0.25, 0.5

let readData path = 
  let lines = path |> CommonTools.IO.readLines |> Seq.toList |> List.tail // Skip header line
  let parseLine (s:string) =
    let array = s.Split([|','|]) 
    double(array.[0]), double(array.[1])
  let pairs = lines |> List.map parseLine
  pairs

let saveArtificialHistory_points path points =
  let pointToString point = 
    sprintf "%A,%A" (fst point) (snd point)
  let lines = "t[s],a[g]" :: List.map pointToString points
  CommonTools.IO.writeLines path lines

let points_original = readData path

let dt =
  match points_original with
  | (t0,_)::(t1,_)::_ -> t1 - t0
  | _ -> failwith "Problem parsing list to get time step."

let (tList, aList) = List.unzip points_original
let (vList, uList) = AdjustAccelerogram.integrateAcceleration (beta, gamma) dt aList

// Plot original
//FSharp.Charting.Chart.Line (List.zip tList aList)
//FSharp.Charting.Chart.Line (List.zip tList vList)
//FSharp.Charting.Chart.Line (List.zip tList uList)

// Adjust
let aList_adj = AdjustAccelerogram.rectifyAccelerogram (beta, gamma) dt aList

// Integrate adjusted
let (vList_adj, uList_adj) = AdjustAccelerogram.integrateAcceleration (beta, gamma) dt aList_adj

CommonTools.List.last vList_adj
CommonTools.List.last uList_adj

[
FSharp.Charting.Chart.Line (List.zip tList aList)
FSharp.Charting.Chart.Line (List.zip tList aList_adj)
] |> FSharp.Charting.Chart.Combine |> FSharp.Charting.Chart.Show

[
FSharp.Charting.Chart.Line (List.zip tList vList)
FSharp.Charting.Chart.Line (List.zip tList vList_adj)
] |> FSharp.Charting.Chart.Combine |> FSharp.Charting.Chart.Show

[
FSharp.Charting.Chart.Line (List.zip tList uList)
FSharp.Charting.Chart.Line (List.zip tList uList_adj)
] |> FSharp.Charting.Chart.Combine |> FSharp.Charting.Chart.Show

saveArtificialHistory_points path_adj (List.zip tList aList_adj)

