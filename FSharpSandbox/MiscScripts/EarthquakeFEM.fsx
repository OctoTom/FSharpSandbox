#r @"c:\Users\Tomas\Sync\Timber\LepenyNosnik\packages\MathNet.Numerics.3.11.0\lib\net40\MathNet.Numerics.dll"
#r @"c:\Users\Tomas\Sync\Timber\LepenyNosnik\packages\MathNet.Numerics.FSharp.3.11.0\lib\net40\MathNet.Numerics.FSharp.dll"
#r @"c:\Users\Tomas\OneDrive\OneSync\Projects\CommonTools\CommonTools\Bin\Release\CommonTools.dll"
#load @"c:\Users\Tomas\Scripts\Paket\LoadPackages.fsx"

open System
open System.Runtime.InteropServices
open System.IO
open System.Numerics // System.Numerics.Complex
open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra
open FSharp.Charting

/// Units are those of SI: m, s, N, kg 

module Types = 
  /// Stores material properties
  type material =
    {
      /// Young's modulus
      E : double
      /// Density
      rho : double
    }

  /// Stores element data
  type element =
    {
      /// Element's length
      L : double
      /// Element's cross section
      A : double
      /// List of code numbers. DOFs are indexed from 0 to nDOF-1.
      cn : int list
      /// Element's material
      material : material
    }

  /// Solution parameters type
  type solParams =
    {
      /// Parameter alpha in Hughes implicit integration scheme.
      alpha : double
      /// Parameter beta in Hughes implicit integration scheme.
      beta : double
      /// Parameter gamma in Hughes implicit integration scheme.
      gamma : double
      /// Time step
      dt : double
    }

open Types

let getK (e : element) =
  let k = e.material.E * e.A / e.L
  let array = [k; -k; -k; k] |> List.toArray
  MathNet.Numerics.LinearAlgebra.Matrix<double>.Build.Dense(2, 2, array)

let getM (e : element) =
  let m = e.material.rho * e.A * e.L
  let array = [m/3.0; m/6.0; m/6.0; m/3.0] |> List.toArray
  MathNet.Numerics.LinearAlgebra.Matrix<double>.Build.Dense(2, 2, array)

let getNumDOF (es : element list) = 
  let getElementMaxCN e = e.cn |> List.max
  let maxCN = es |> List.maxBy getElementMaxCN |> getElementMaxCN
  maxCN + 1

/// Localizes element matrices to global matrix.
/// Mapping selects the matrices which are to be localized, i.e. stiffness or mass matrix.
let localize (es : element list) (mapping : element -> Matrix<double>) =
  let nDOF = getNumDOF es
  let matrix = MathNet.Numerics.LinearAlgebra.Matrix<double>.Build.Dense(nDOF, nDOF)
  let localizeElement e =
    let elemNumDOF = e.cn.Length
    let elemMatrix = mapping e
    let action i j =
      matrix.[e.cn.[i], e.cn.[j]] <- matrix.[e.cn.[i], e.cn.[j]] + elemMatrix.[i, j]
    for i in [0 .. elemNumDOF - 1] do 
      for j in [0 .. elemNumDOF - 1] do
        action i j
  es |> List.iter localizeElement
  matrix

let m1 = {E = 1000.0; rho = 2000.0}
let e1 : element = { material = m1; A = 1.0; L = 1.0; cn = [0; 1]}
let e2 : element = { material = m1; A = 1.0; L = 2.0; cn = [1; 2]}

let es = [e1; e2]
let globalK = localize es getK
let globalM = localize es getM
globalK.ConditionNumber()
globalM.ConditionNumber()

/// Time step
let dt = 0.01 // s
/// Prescribed accelerogram
let a_I0 = [0.0; 0.0; 1.0; 0.0; 0.0; -1.0; 0.0; 0.0; 0.0; 0.0]

/// Values of the solution parameters
let sp =
  let alpha = 0.0
  //let alpha = -1.0 / 3.0
  {
    alpha = alpha
    beta = (1.0 - alpha)**2.0 / 4.0
    gamma = (1.0 - 2.0 * alpha) / 2.0
    dt = 0.01
  }

let sp' =
  //let alpha = 0.0
  let alpha = -1.0 / 3.0
  {
    alpha = alpha
    beta = (1.0 - alpha)**2.0 / 4.0
    gamma = (1.0 - 2.0 * alpha) / 2.0
    dt = 0.01
  }

/// Integrates acceleration and generates 
let integrateAcceleration (beta, gamma) dt aList =
  let folder (a : double list, v : double list, d : double list) a1 =
    match a, v, d with
    | a0 :: _, v0 :: _, d0 :: _ ->
      let v1 = v0 + dt * ((1.0 - gamma) * a0 + gamma * a1)
      let d1 = d0 + dt * v0 + dt * dt / 2.0 * ((1.0 - 2.0 * beta) * a0 + 2.0 * beta * a1)
      a1 :: a, v1 :: v, d1 :: d
    | _ -> [a1], [0.0], [0.0] 
  let a, v, d = List.fold folder ([], [], []) aList // Value a is the same as aList. No need to return it.
  List.rev v, List.rev d

let v_I0, d_I0 = integrateAcceleration (sp.beta, sp.gamma) sp.dt a_I0
let v_I0', d_I0' = integrateAcceleration (sp'.beta, sp'.gamma) sp'.dt a_I0

Chart.Line a_I0
[Chart.Line v_I0; Chart.Line v_I0'] |> Chart.Combine
[Chart.Line d_I0; Chart.Line d_I0'] |> Chart.Combine

