#load @"c:\Users\Tomas\Scripts\Paket\References.fsx"


open System
open System.Runtime.InteropServices
open System.IO
open System.Numerics // System.Numerics.Complex
open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra // Vector<>, Matrix<>
open FSharp.Charting // Chart

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
      /// Time
      t : float
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
      /// Solution parameters.
      sp : solParams
      /// Parameter alpha used for a_I0 and v_I0 in the Hughes' alpha-method.
      alpha_I0 : float
      /// Parameter beta used in a_I0 integration.
      beta_I0 : float
      /// Parameter gamma used in a_I0 integration.
      gamma_I0 : float
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
      /// Input signal a, v and d. The intermediate (alpha) values.
      inputSignal_alpha : (float * float * float) list
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
  let array = [m / 3.0; m / 6.0; m / 6.0; m / 3.0] |> List.toArray // Full consistent mass matrix
  //let array = [m / 2.0; 0.0; 0.0; m / 2.0] |> List.toArray // Lumped diagonal mass matrix
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
  let matrix = Matrix<float>.Build.Sparse(nDOF, nDOF)
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

/// Save displacement fields in each timestep in
let savePlotsWithFields folderPath takeEvery max coords fields =
  // Delete existing .png files
  (new DirectoryInfo(folderPath)).EnumerateFiles("*.png") |> Seq.iter (fun file -> file.Delete())
  let action i x =
    if i % takeEvery = 0 then
      let data = x.d |> Seq.toList
      List.zip coords data
      |> Chart.FastLine
      |> Chart.WithYAxis(Max = max, Min = -max)
      |> Chart.Save(sprintf "%sdisp%04d.png" folderPath i)
    else ()
  fields |> List.iteri action 



/// Main analysis driver
let solve input =
  printfn "Solve started."
  let es = input.es
  let sp = input.sp
  let accelerogramTimeStep = input.accelerogramTimeStep
  let accelerogram = input.accelerogram
  let alpha_I0, beta_I0, gamma_I0 = input.alpha_I0, input.beta_I0, input.gamma_I0
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
    let m = es.Head.material
    let c = sqrt(m.E / m.rho)
    let matrix = MathNet.Numerics.LinearAlgebra.Matrix<float>.Build.Sparse(nDOF, nDOF)
    matrix.[0, 0] <- m.E / c
    matrix
  /// LHS matrix of the global system of linear equations.
  let matrixA =
    let alpha, beta, gamma = sp.alpha, sp.beta, sp.gamma
    let dt = sp.dt
    matrixM + (1.0 + alpha) * gamma * dt * matrixC + (1.0 + alpha) * beta * dt**2.0 * matrixK
  /// Evaluates the RHS for given values of prescribed acceleration and velocity
  /// and the values of an, vn and dn from the previous step.
  let alpha, beta, gamma = sp.alpha, sp.beta, sp.gamma
  let dt = sp.dt
  let helperMatrix1 = (1.0 + alpha) * ((1.0 - gamma) * dt * matrixC + (1.0 - 2.0 * beta) * dt**2.0 / 2.0 * matrixK)
  let helperMatrix2 = matrixC + (1.0 + alpha) * dt * matrixK
  let getRHS (a_I0 : Vector<float>, v_I0 : Vector<float>, an : Vector<float>, vn : Vector<float>, dn : Vector<float>) =
    -matrixM * a_I0 + matrixC * v_I0 - helperMatrix1 * an - helperMatrix2 * vn - matrixK * dn
    //-matrixM * a_I0 - helperMatrix1 * an - helperMatrix2 * vn - matrixK * dn

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

  let a_I0 = accelerogram
  let v_I0, d_I0 = integrateAcceleration (beta_I0, gamma_I0) sp.dt accelerogram 
  
  /// Returns list of values expressed at t_(n+alpha)
  let getAlphaValues alpha list =
    let mapNeighbours mapping list =
      let listOfFst = list |> List.rev |> List.tail |> List.rev
      let listOfSnd = list |> List.tail
      List.map2 mapping listOfFst listOfSnd 
    list |> mapNeighbours (fun x0 x1 -> (1.0 + alpha) * x1 - alpha * x0)

  /// LOOP OF HHT ALPHA-METHOD
  /// Initial state. It consists of zero nodal a, v and d.
  let initState : unknowns list =
    let numDOF = getNumDOF es
    { 
      t = 0.0
      a = Vector<float>.Build.Dense(numDOF)
      v = Vector<float>.Build.Dense(numDOF)
      d = Vector<float>.Build.Dense(numDOF)
    } :: []
  /// The folder which incrementally builds results.
  let folder (state : unknowns list) (x : float * float) =
    do sprintf "The folder called for t = %A." state.Head.t
    let aI0, vI0 = x // Loading values
    let beta, gamma = sp.beta, sp.gamma
    let dt = sp.dt
    match state with
    | {t = t0; a = a0; v = v0; d = d0} :: tail ->
      let aI0Vec = aI0 * Vector<float>.Build.Dense(numDOF, (fun i -> 1.0))
      let vI0Vec = vI0 * Vector<float>.Build.Dense(numDOF, (fun i -> 1.0))
      let rhs = getRHS (aI0Vec, vI0Vec, a0, v0, d0)
      let t1 = t0 + dt
      let a1 = matrixA.Solve(rhs)
      let v1 = v0 + dt * ((1.0 - gamma) * a0 + gamma * a1)
      let d1 = d0 + dt * v0 + dt * dt / 2.0 * ((1.0 - 2.0 * beta) * a0 + 2.0 * beta * a1)
      //printfn "%A" rhs
      //printfn "%A" matrixA
      //printfn "%A" a1
      //printfn "%A" v1
      //printfn "%A" d1
      {t = t1; a = a1; v = v1; d = d1} :: state
    | _ -> failwith "Empty list of unknowns."
  /// Unfold the time series of a, v and d fields (represented as vectors).
  /// The values are the u_r, i.e. the values relative to the incomming wave signal.
  printfn "Folding started."
  let results = List.fold folder initState (List.zip a_I0 v_I0) |> List.rev
  printfn "Folding finished."
 
  /// Time series of total fields a, v and d.
  let totalResults =
    /// Takes unknowns (i.e. vectors a, v and d) and adds the incoming wave value at this time step.
    let mapping (u : unknowns) (aI0, vI0, dI0) =
      let t, a, v, d = u.t, u.a, u.v, u.d
      /// Total value
      let aT = a |> Vector.map (fun x -> x + aI0)
      let vT = v |> Vector.map (fun x -> x + vI0)
      let dT = d |> Vector.map (fun x -> x + dI0)
      {t = t; a = aT; v = vT; d = dT}
    let avd_I0 = List.zip3 a_I0 v_I0 d_I0
    //if results.Length <> avd_I0.Length then failwith (sprintf "results.Length = %A, avd_I0.Length = %A" results.Length avd_I0.Length)
    List.map2 mapping (results |> List.rev |> List.tail |> List.rev) avd_I0
  /// Return output record
  {relativeFields = results; totalFields = totalResults; inputSignal = (List.zip3 a_I0 v_I0 d_I0); inputSignal_alpha = (List.zip3 a_I0 v_I0 d_I0); numDof = numDOF}
 

/// Returns wave speed for material
let getVaveSpeed (m : material) = sqrt(m.E / m.rho)
/// Returns effective element length
let getEffectiveElementLength (m : material) dt courantNumber = dt * getVaveSpeed m / courantNumber


//============//
// Input data // 
//============//

let input = 
  // Material
  let m1 = {E = 5.0e8; rho = 2.0e3}
  let alpha_I0 = 0.0 // -0.3333 -- 0.0
  let beta_I0 = (1.0 - alpha_I0)**2.0 / 4.0 //  0.4444 -- 0.25
  let gamma_I0 = (1.0 - 2.0 * alpha_I0) / 2.0 //  0.8333 -- 0.5
  //
  let timeStep = 0.01
  // Return record of type "input"
  {
    /// List of elements
    es = 
      [0 .. 3] |> List.map (fun i -> { L = 2.0; A = 1.0; cn = [i; i+1]; material = m1})
    /// Prescribed accelerogram time step
    accelerogramTimeStep = 0.01
    /// Prescribed accelerogram data
    accelerogram = [0.0; 1.0; -2.0; 1.0; 0.0] @ List.init 80 (fun _ -> 0.0)
    /// Solution parameters
    sp =
      // Alpha = -1/3 works better than 0.
      let alpha = 0.0 // Parameter alpha belongs to <-1/3, 0>
      {
        alpha = alpha
        beta = (1.0 - alpha)**2.0 / 4.0
        gamma = (1.0 - 2.0 * alpha) / 2.0
        dt = timeStep
      }
    alpha_I0 = alpha_I0
    beta_I0 = beta_I0
    gamma_I0 = gamma_I0
  } // End of input

let numElems = input.es.Length
let totalTime = float input.accelerogram.Length * input.accelerogramTimeStep // s
let numTimeSteps = totalTime / input.sp.dt 

/// Run the analysis for given input
let output = solve input

let step = 2
printfn "%A" output.relativeFields.[step].t
printfn "%A" (output.relativeFields.[step].a |> Seq.toList)
printfn "%A" (output.relativeFields.[step].v |> Seq.toList)
printfn "%A" (output.relativeFields.[step].d |> Seq.toList)
printfn "%A" (output.inputSignal.[step] |> CommonTools.Misc.first)
printfn "%A" (output.inputSignal.[step] |> CommonTools.Misc.second)
printfn "%A" (output.inputSignal.[step] |> CommonTools.Misc.third)
printfn "%A" (output.totalFields .[step].a |> Seq.toList)
printfn "%A" (output.totalFields.[step].v |> Seq.toList)
printfn "%A" (output.totalFields.[step].d |> Seq.toList)

/// Nodal coordinates computed from elements lengths
let nodalCoords = input.es |> List.fold (fun state e -> state.Head + e.L :: state) [0.0] |> List.rev

/// Save .png plots
do savePlotsWithFields "c:/Users/Tomas/Temp/" 1 0.0001 nodalCoords output.totalFields
/// Save .png plots
//do savePlotsWithFields "c:/Users/Tomas/Temp/" 5 0.0001 nodalCoords output.relativeFields

/// Plot time evolution of displacement in all nodes
[for i in 0 .. 1 .. output.totalFields.Head.a.Count - 1
  -> Chart.FastLine(output.relativeFields |> List.map (fun x -> x.d.[i]), Name=sprintf "Node %A" i)
] |> Chart.Combine |> Chart.WithLegend(true) |> Chart.WithXAxis(Title = "t [s]")

/// Input signal. Acceleration, velocity and displacement. Ad hoc scaling.
let printInputSignal =
  let takeFirst = 40
  let inputSignal = output.inputSignal |> List.take takeFirst
  let inputSignal_alpha = output.inputSignal_alpha |> List.take takeFirst
  [
    Chart.FastLine(inputSignal |> List.map (fun (a, _, _) -> a), Name= "a_I0")
    Chart.FastLine(inputSignal |> List.map (fun (_, v, _) -> 100.0*v), Name= "v_I0")
    Chart.FastLine(inputSignal |> List.map (fun (_, _, d) -> 10000.0*d), Name= "d_I0")
    Chart.FastLine(inputSignal_alpha |> List.map (fun (a, _, _) -> a), Name= "a_I0a")
    Chart.FastLine(inputSignal_alpha |> List.map (fun (_, v, _) -> 100.0*v), Name= "v_I0a")
    Chart.FastLine(inputSignal_alpha |> List.map (fun (_, _, d) -> 10000.0*d), Name= "d_I0a")
  ] |> Chart.Combine |> Chart.WithLegend(true)





