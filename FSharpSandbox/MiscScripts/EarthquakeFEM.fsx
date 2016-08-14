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

/// SI units are used in this script (m, s, kg, N, Pa).

module Types = 
  /// Stores material properties
  type material =
    {
      /// Young's modulus [Pa]
      E : float
      /// Density [kg/m3]
      rho : float
    }
  /// Stores element data
  type element =
    {
      /// Element's length [m]
      L : float
      /// Element's cross section [m2]
      A : float
      /// List of code numbers. DOFs are indexed from 0 to nDOF-1.
      cn : int list
      /// Element's material
      material : material
    }
  /// Solution parameters type
  type solParams =
    {
      /// Parameter alpha in Hughes implicit integration scheme.
      alpha : float
      /// Parameter beta in Hughes implicit integration scheme.
      beta : float
      /// Parameter gamma in Hughes implicit integration scheme.
      gamma : float
      /// Time step used in Hughes integration.
      /// (Prescribed accelerogram may have different time step)
      dt : float
    }
  /// Fields a, v and d in single time step.
  type unknowns =
    {
      /// Nodal acceleration
      a : Vector<float>
      /// Nodal velocity
      v : Vector<float>
      /// Nodal displacement
      d : Vector<float>
    }
  /// All input data entering the analysis
  type input =
    {
      /// List of elements.
      es : element list
      /// Accelerogram in form of time series with constant time step size.
      accelerogram : float list
      /// Size of accelerogram's time step [s].
      accelerogramTimeStep : float
      /// Solution parameters
      sp : solParams
    }
  /// Record containing all information returning from the analysis. 
  type output =
    {
      /// Relative field (relative to the input signal) time series.
      relativeFields : unknowns list
      /// Total field, i.e. relative plus input time series.
      totalFields : unknowns list
      /// Input signal a, v and d. 
      inputSignal : (float * float * float) list
      /// Number of degrees of freedom.
      numDof : int
    }
// End of Types module.

open Types

/// Returns stiffness matrix of given element
let getK (e : element) =
  let k = e.material.E * e.A / e.L
  let array = [k; -k; -k; k] |> List.toArray
  Matrix<float>.Build.Dense(2, 2, array)
/// Returns mass matrix of given element
let getM (e : element) =
  let m = e.material.rho * e.A * e.L
  let array = [m / 3.0; m / 6.0; m / 6.0; m / 3.0] |> List.toArray
  Matrix<float>.Build.Dense(2, 2, array)
/// Returns the number of DOFs as the highest code number plus one.
let getNumDOF (es : element list) = 
  let getElementMaxCN e = e.cn |> List.max
  let maxCN = es |> List.maxBy getElementMaxCN |> getElementMaxCN
  maxCN + 1

/// Localizes element matrices to global matrix.
/// Mapping selects the matrices which are to be localized, i.e. stiffness or mass matrix.
let localize (es : element list) (mapping : element -> Matrix<float>) =
  let nDOF = getNumDOF es
  let matrix = Matrix<float>.Build.Dense(nDOF, nDOF)
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

/// Takes time series with constant time step DT
/// and generates time series with finner step dt. 
let interpolate DT (data : float list) dt =
  let scale = DT / dt
  let N = data.Length
  let initializer i =
    let index = float i / scale |> int // Whole part of the number
    if index < N - 1 then
      let fraction = float i / scale - float index
      Some (data.[index] + fraction * (data.[index + 1] - data.[index]))
    else
      None
  Seq.initInfinite initializer |> Seq.takeWhile (fun x -> x.IsSome) |> Seq.choose id |> Seq.toList


/// Main analysis driver
let solve input =
  let es = input.es
  let sp = input.sp
  let accelerogramTimeStep = input.accelerogramTimeStep
  let accelerogram = input.accelerogram
  /// Number of degrees of freedom
  let numDOF = getNumDOF es
  /// Global stiffness matrix
  let matrixK = localize es getK
  /// Global mass matrix
  let matrixM = localize es getM
  /// Global dumping matrix.
  /// Here the dumping is only attributed to the boundary condition at the lowest node (with c.n.=0).
  let matrixC =
    let nDOF = getNumDOF es
    let E = es.Head.material.E
    let rho = es.Head.material.rho
    let c = sqrt(E / rho)
    let matrix = Matrix<float>.Build.Dense(nDOF, nDOF)
    matrix.[0, 0] <- E / c
    matrix
  /// LHS matrix of the global system of linear equations.
  let matrixA =
    let alpha, beta, gamma = sp.alpha, sp.beta, sp.gamma
    let dt = sp.dt
    matrixM + (1.0 + alpha) * gamma * dt * matrixC + (1.0 + alpha) * beta * dt**2.0 * matrixK
  /// Evaluates the RHS for given values of prescribed acceleration and velocity
  /// and the values of an, vn and dn from the previous step.
  /// QUESTION: Are the presctibed values expressed in t_(n+alpha) time?
  let getRHS (a_I0 : Vector<float>, v_I0 : Vector<float>, an : Vector<float>, vn : Vector<float>, dn : Vector<float>) =
    let alpha, beta, gamma = sp.alpha, sp.beta, sp.gamma
    let dt = sp.dt
    let helperMatrix1 = (1.0 + alpha) * ((1.0 - gamma) * dt * matrixC + (1.0 - 2.0 * beta) * dt**2.0 / 2.0 * matrixK)
    let helperMatrix2 = matrixC + (1.0 + alpha) * dt * matrixK
    -matrixM * a_I0 + matrixC * v_I0 - helperMatrix1 * an - helperMatrix2 * vn - matrixK * dn

  /// Integrates accelerogram (with equidistant points)
  /// and generates time series of velocity and displacement (both starting at 0.0).
  let integrateAcceleration (beta, gamma) dt aList =
    let folder (a : float list, v : float list, d : float list) a1 =
      match a, v, d with
      | a0 :: _, v0 :: _, d0 :: _ ->
        let v1 = v0 + dt * ((1.0 - gamma) * a0 + gamma * a1)
        let d1 = d0 + dt * v0 + dt * dt / 2.0 * ((1.0 - 2.0 * beta) * a0 + 2.0 * beta * a1)
        a1 :: a, v1 :: v, d1 :: d
      | _ -> [a1], [0.0], [0.0] 
    let a, v, d = List.fold folder ([], [], []) aList // Value a is the same as aList. No need to return it.
    List.rev v, List.rev d

  /// Time series of velocity loading.
  let a_I0 = interpolate accelerogramTimeStep accelerogram sp.dt
  let v_I0, d_I0 = integrateAcceleration (sp.beta, sp.gamma) sp.dt a_I0 // 

  /// Returns list of values expressed at t_(n+alpha)
  let getAlphaValues alpha list =
    let rec loop list =
      match list with
      | x0 :: x1 :: tail -> ((1.0 + alpha) * x1 - alpha * x0) :: (loop (x1 :: tail))
      | _ -> []
    loop list

  /// List of intermediate (n+beta) values of prescribed acceleration.
  let a_I0alpha = getAlphaValues sp.alpha a_I0
  /// List of intermediate (n+beta) values of prescribed velocity.
  let v_I0alpha = getAlphaValues sp.alpha v_I0

    /// Initial state. It consists of 
  let initState : unknowns list =
    let numDOF = getNumDOF es
    {
      a = Vector<float>.Build.Dense(numDOF)
      v = Vector<float>.Build.Dense(numDOF)
      d = Vector<float>.Build.Dense(numDOF)
    } :: []
  /// The folder which incrementally builds results.
  let folder (state : unknowns list) (x : float * float) =
    let beta, gamma = sp.beta, sp.gamma
    let dt = sp.dt
    match state with
    | {a = a0; v = v0; d = d0} :: tail ->
      let aI0Vec = (fst x) * Vector<float>.Build.Dense(numDOF, (fun i -> 1.0))
      let vI0Vec = (snd x) * Vector<float>.Build.Dense(numDOF, (fun i -> 1.0))
      let rhs = getRHS (aI0Vec, vI0Vec, a0, v0, d0)
      let a1 = matrixA.Solve(rhs)
      let v1 = v0 + dt * ((1.0 - gamma) * a0 + gamma * a1)
      let d1 = d0 + dt * v0 + dt * dt / 2.0 * ((1.0 - 2.0 * beta) * a0 + 2.0 * beta * a1)
      {a = a1; v = v1; d = d1} :: state
    | _ -> failwith "Empty list of unknowns."

  /// Unfold the time series of a, v and d fields (represented as vectors).
  /// The values are the u_r, i.e. the values relative to the incomming wave signal.
  let results = List.fold folder initState (List.zip a_I0alpha v_I0alpha) |> List.rev
  //let results = List.fold folder initState (List.zip a_I0 v_I0) |> List.rev

  /// Time series of total fields a, v and d.
  let totalResults =
    /// Takes unknowns (i.e. vectors a, v and d) and adds the incoming wave value at this time step.
    let mapping (u : unknowns) (aI0, vI0, dI0) =
      let a, v, d = u.a, u.v, u.d
      /// Total value
      let aT = a |> Vector.map (fun x -> x + aI0)
      let vT = v |> Vector.map (fun x -> x + vI0)
      let dT = d |> Vector.map (fun x -> x + dI0)
      {a = aT; v = vT; d = dT}
    let avd_I0 = List.zip3 a_I0 v_I0 d_I0
    //if results.Length <> avd_I0.Length then failwith (sprintf "results.Length = %A, avd_I0.Length = %A" results.Length avd_I0.Length)
    List.map2 mapping results avd_I0
  /// Return output record
  {relativeFields = results; totalFields = totalResults; inputSignal = (List.zip3 a_I0 v_I0 d_I0); numDof = numDOF}
  

//============//
// Input data // 
//============//
let input = 
  // Material
  let m1 = {E = 2.0e9; rho = 2.0e3}
  // Return record of type "input"
  {
    /// List of elements
    es = List.init 100 (fun i -> { material = m1; A = 1.0; L = 0.5; cn = [i; i + 1]})
    /// Prescribed accelerogram time step
    accelerogramTimeStep = 0.01
    /// Prescribed accelerogram data
    accelerogram = [0.0; 1.0; -2.0; 1.0; 0.0] @ [for i in [0..10] -> 0.0]
    /// Solution parameters
    sp =
      let alpha = 0.0
      //let alpha = -1.0 / 3.0
      {
        alpha = alpha
        beta = (1.0 - alpha)**2.0 / 4.0
        //beta = 0.0
        gamma = (1.0 - 2.0 * alpha) / 2.0
        //gamma = 0.0
        dt = 0.001
      }
  }

/// Run the analysis for given input
let result = solve input

/// Plot time evolution of displacement in all nodes
[for i in 0 .. 10 .. result.totalFields.Head.a.Count - 1
  -> Chart.FastLine(result.totalFields |> List.map (fun x -> x.d.[i]), Name=sprintf "%A" i)
] |> Chart.Combine |> Chart.WithLegend(true)

/// Plot time evolution of acceleration in all nodes
[for i in 0 .. 10 .. result.totalFields.Head.a.Count - 1
  -> Chart.FastLine(result.totalFields |> List.map (fun x -> x.a.[i]), Name=sprintf "%A" i)
] |> Chart.Combine |> Chart.WithLegend(true)

/// Ad hoc scaled.
[
  Chart.Line(result.inputSignal |> List.map (fun (a, _, _) -> a), Name= "a_I0")
  Chart.Line(result.inputSignal |> List.map (fun (_, v, _) -> 100.0*v), Name= "v_I0")
  Chart.Line(result.inputSignal |> List.map (fun (_, _, d) -> 10000.0*d), Name= "d_I0")
] |> Chart.Combine |> Chart.WithLegend(true)

