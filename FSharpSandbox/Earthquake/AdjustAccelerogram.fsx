#load @"c:\Users\Tomas\Scripts\Paket\References.fsx"

open System
open System.Runtime.InteropServices
open System.IO
open System.Numerics // System.Numerics.Complex
open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra // Vector<>, Matrix<>
open FSharp.Charting // Chart

// SI units are used in this script (m, s, kg, N, Pa).

// Artificially generated spectrum-compatible accelerograms do not integrate to zero,
// i.e. the velocity and displacement at the end of earthquake are not zero.
// This file contains test to adjust these accelerograms.

/// Integrates accelerogram (with equidistant points)
/// and generates time series of velocity and displacement (both starting at 0.0).
/// Standard parameters are beta = 0.25 and gamma = 0.5 
let integrateAcceleration (beta, gamma) dt aList =
  let folder (a : float list, v : float list, d : float list) a1 =
    match a, v, d with
    | a0 :: _, v0 :: _, d0 :: _ ->
      //printfn "%A, %A, %A" a0 v0 d0
      //printfn "%A, %A, %A" dt gamma beta
      let v1 = v0 + dt * ((1.0 - gamma) * a0 + gamma * a1)
      let d1 = d0 + dt * v0 + dt * dt / 2.0 * ((1.0 - 2.0 * beta) * a0 + 2.0 * beta * a1)
      a1 :: a, v1 :: v, d1 :: d
    | _ -> [a1], [0.0], [0.0] // Empty
  let a, v, d = List.fold folder ([], [], []) aList // Value a is the same as aList. No need to return it.
  List.rev v, List.rev d

/// Returns pair (a, b) parameters of the linear accelerogram
/// that integrte to velocity vT and displacement uT at time T.
/// We asume initial velocity v0 = 0 and displacement d0 = 0.
let getAB T vT uT = 
  let b = -12.0/T/T/T*uT + 6.0/T/T*vT
  let a = 1.0/T*vT - 1.0/2.0*T*b
  (a, b)

/// Creates linear accelerogram given num_points, step size and parameters A and B.
let getAccelerogramAB length dt (a, b) =
  let aFunc t = a + b*t // Linear function a(t)
  let i2t i = float i * dt // Converts i to time
  let aList = List.init length (i2t >> aFunc)
  aList

/// Modified accelerogram given in form of aList so that it integrates
/// to zero velocity and zero displacement at the end.
/// Default values are (beta, gamma) = (0.25, 0.5).
let rectifyAccelerogram (beta, gamma) dt aList = 
  let length = List.length aList // Number of points
  let T = float (length-1) * dt
  // Integrate numerically
  let (vList, uList) = integrateAcceleration (beta, gamma) dt aList
  // Get final velocity and final displacement
  let vT = vList |> List.rev |> List.head
  let uT = uList |> List.rev |> List.head
  // Parameters a and b computed for the random accelerogram.
  let (a_cor, b_cor) = getAB T vT uT
  // Correcting accelerogram
  let aList_cor = getAccelerogramAB length dt (a_cor, b_cor)
  // Corrected accelerogram
  let aList_new = List.map2 (-) aList aList_cor
  aList_new


// TEST:

(*
// Random series of values in (-1.0, 1.0).
let randomSymmetricSeries seed =
  let rnd = System.Random(seed)
  Seq.initInfinite (fun _ -> 2.0 * rnd.NextDouble() - 1.0) |> Seq.cache

// Random seed
let seed = 42

// Integration parameters
let (beta, gamma) = 0.25, 0.5

// Time step
let dt = 0.5

// Number of points
let length = 11

// Duration of earthquake
let T = float (length - 1) * dt

// Random accelerogram
let aList = randomSymmetricSeries seed |> Seq.take length |> Seq.toList
// Integrate numerically
let (vList, uList) = integrateAcceleration (beta, gamma) dt aList

// Visualize it
[
FSharp.Charting.Chart.Line(aList)
FSharp.Charting.Chart.Line(vList)
FSharp.Charting.Chart.Line(uList)
] |> FSharp.Charting.Chart.Combine |> FSharp.Charting.Chart.Show


// Corrected accelerogram
let aList_new = rectifyAccelerogram (beta, gamma) dt aList

// Integrate numerically
let (vList_new, uList_new) = integrateAcceleration (beta, gamma) dt aList_new

// Visualize it
[
FSharp.Charting.Chart.Line(aList_new)
FSharp.Charting.Chart.Line(vList_new)
FSharp.Charting.Chart.Line(uList_new)
] |> FSharp.Charting.Chart.Combine |> FSharp.Charting.Chart.Show

*)



