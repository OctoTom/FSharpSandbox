﻿#load @"c:\Users\Tomas\Scripts\Paket\References.fsx"
open System
open FSharp.Charting
open CommonTools

/// Generates envelope function, see Kumar's paper.
/// Eta constols amplitude at the end of series.
/// Epsilon controlls the position of maximum within the duration time.
/// Tw is the duration time.
let getEnvelopeFunc eta epsilon tw =
  let b = -epsilon * Math.Log eta / (1.0 + epsilon * (Math.Log epsilon - 1.0))
  let c = b / epsilon / tw
  let a = (Math.E / epsilon / tw)**b
  let func t = a * t**b * Math.Exp (-c * t)
  func
// Kumar's values
let envelopeFunc = getEnvelopeFunc 0.01 0.3 20.0
// Plot it.
[0.0 .. 0.1 .. 20.0] |> List.mapio envelopeFunc |> Chart.FastLine |> Chart.WithXAxis(Min = 0.0)


