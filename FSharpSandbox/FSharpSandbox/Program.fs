namespace Program

open System
open System.Runtime.InteropServices
open System.IO
open System.Numerics // System.Numerics.Complex
open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Double // Vector<>, Matrix<>
open FSharp.Charting // Chart

/// SI units are used in this script (m, s, kg, N, Pa).

module Types =
  open MathNet.Numerics.LinearAlgebra // Vector<>, Matrix<>
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
      a : Vector
      /// Nodal velocity
      v : Vector
      /// Nodal displacement
      d : Vector
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

module Earthquake = 
  /// Returns stiffness matrix of given element
  let getK (e : element) =
    let k = e.material.E * e.A / e.L
    let array = [k; -k; -k; k] |> List.toArray
    DenseMatrix.OfColumnMajor(2,2, array)
  /// Returns mass matrix of given element
  let getM (e : element) =
    let m = e.material.rho * e.A * e.L
    let array = [m / 3.0; m / 6.0; m / 6.0; m / 3.0] |> List.toArray // Full consistent mass matrix
    //let array = [m / 2.0; 0.0; 0.0; m / 2.0] |> List.toArray // Lumped diagonal mass matrix
    DenseMatrix.OfColumnMajor(2,2, array)
  /// Returns the number of DOFs as the highest code number plus one.
  let getNumDOF (es : element list) = 
    let getElementMaxCN e = e.cn |> List.max
    let maxCN = es |> List.maxBy getElementMaxCN |> getElementMaxCN
    maxCN + 1

  /// Localizes element matrices to global matrix.
  /// Mapping selects the matrices which are to be localized, i.e. stiffness or mass matrix.
  let localize (es : element list) (mapping : element -> DenseMatrix) =
    let nDOF = getNumDOF es
    let matrix = DenseMatrix.Create(nDOF, nDOF, fun i j -> 0.0)
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
//  let savePlotsWithTotalFields folderPath takeEvery fields =
//    (new DirectoryInfo(folderPath)).EnumerateFiles("*.png") |> Seq.iter (fun file -> file.Delete())
//    let action i x =
//      if i % takeEvery = 0 then
//        x.d
//        |> Seq.toList
//        |> Chart.FastLine
//        |> Chart.WithYAxis(Max = 2.0e-4, Min = -2.0e-4)
//        |> Chart.Save(sprintf "%sdisp%04d.png" folderPath i)
//      else ()
//    fields|> List.iteri action 



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
      let E = es.Head.material.E
      let rho = es.Head.material.rho
      let c = sqrt(E / rho)
      let matrix = MathNet.Numerics.LinearAlgebra.Double.DenseMatrix.Create(nDOF, nDOF, fun i j -> 0.0)
      matrix.[0, 0] <- E / c
      matrix
    /// LHS matrix of the global system of linear equations.
    let matrixA =
      let alpha, beta, gamma = sp.alpha, sp.beta, sp.gamma
      let dt = sp.dt
      matrixM + (1.0 + alpha) * gamma * dt * matrixC + (1.0 + alpha) * beta * dt**2.0 * matrixK
    /// Evaluates the RHS for given values of prescribed acceleration and velocity
    /// and the values of an, vn and dn from the previous step.
    let getRHS (a_I0 : Vector, v_I0 : Vector, an : Vector, vn : Vector, dn : Vector) =
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
    printfn "Interpolate acceleration started."
    let a_I0 = interpolate accelerogramTimeStep accelerogram sp.dt
    printfn "Integrate acceleration started."
    let v_I0, d_I0 = integrateAcceleration (beta_I0, gamma_I0) sp.dt a_I0 // 

    /// Returns list of values expressed at t_(n+alpha)
    let getAlphaValues alpha list =
      let mapNeighbours mapping list =
        let listOfFst = list |> List.rev |> List.tail |> List.rev
        let listOfSnd = list |> List.tail
        List.map2 mapping listOfFst listOfSnd 
      list |> mapNeighbours (fun x0 x1 -> (1.0 + alpha) * x1 - alpha * x0)

    /// List of intermediate (n+beta) values of prescribed acceleration.
    printfn "GetAlphaValues started."
    let a_I0alpha = getAlphaValues alpha_I0 a_I0
    /// List of intermediate (n+beta) values of prescribed velocity.
    printfn "GetAlphaValues started."
    let v_I0alpha = getAlphaValues alpha_I0 v_I0
    /// List of intermediate (n+beta) values of prescribed displacement. (not needed for solution, just for completeness)
    printfn "GetAlphaValues started."
    let d_I0alpha = getAlphaValues alpha_I0 d_I0

    /// Initial state. It consists of 
    let initState : unknowns list =
      let numDOF = getNumDOF es
      {
        a = DenseVector.Create(numDOF, fun i -> 0.0)
        v = DenseVector.Create(numDOF, fun i -> 0.0)
        d = DenseVector.Create(numDOF, fun i -> 0.0)
      } :: []
    /// The folder which incrementally builds results.
    let folder (state : unknowns list) (x : float * float) =
      let beta, gamma = sp.beta, sp.gamma
      let dt = sp.dt
      match state with
      | {a = a0; v = v0; d = d0} :: tail ->
        let aI0Vec = (fst x) * DenseVector.Create(numDOF, fun i -> 1.0)
        let vI0Vec = (snd x) * DenseVector.Create(numDOF, fun i -> 1.0)
        let rhs = getRHS (aI0Vec, vI0Vec, a0, v0, d0)
        let a1 = matrixA.Cholesky().Solve(rhs) :?> DenseVector
        let v1 = (v0 + dt * ((1.0 - gamma) * a0 + gamma * a1)) :?> DenseVector
        let d1 = d0 + dt * v0 + dt * dt / 2.0 * ((1.0 - 2.0 * beta) * a0 + 2.0 * beta * a1) :?> DenseVector
        {a = a1; v = v1; d = d1} :: state
      | _ -> failwith "Empty list of unknowns."

    /// Unfold the time series of a, v and d fields (represented as vectors).
    /// The values are the u_r, i.e. the values relative to the incomming wave signal.
    printfn "Folding started."
    let results = List.fold folder initState (List.zip a_I0alpha v_I0alpha) |> List.rev
    printfn "Folding finished."
    //let results = List.fold folder initState (List.zip a_I0 v_I0) |> List.rev

    /// Time series of total fields a, v and d.
    let totalResults =
      /// Takes unknowns (i.e. vectors a, v and d) and adds the incoming wave value at this time step.
      let mapping (u : unknowns) (aI0, vI0, dI0) =
        let a, v, d = u.a, u.v, u.d
        /// Total value
        let aT = a |> Vector.map (fun x -> x + aI0) :?> Vector
        let vT = v |> Vector.map (fun x -> x + vI0) :?> Vector
        let dT = d |> Vector.map (fun x -> x + dI0) :?> Vector
        {a = aT; v = vT; d = dT}
      let avd_I0 = List.zip3 a_I0 v_I0 d_I0
      //if results.Length <> avd_I0.Length then failwith (sprintf "results.Length = %A, avd_I0.Length = %A" results.Length avd_I0.Length)
      List.map2 mapping results avd_I0
    /// Return output record
    {relativeFields = results; totalFields = totalResults; inputSignal = (List.zip3 a_I0 v_I0 d_I0); inputSignal_alpha = (List.zip3 a_I0alpha v_I0alpha d_I0alpha); numDof = numDOF}
 

  //============//
  // Input data // 
  //============//
  let input = 
    // Material
    let m1 = {E = 2.0e8; rho = 2.0e3}
    let m2 = {E = 5.0e7; rho = 2.0e3}
    let numElem = 100
    let alpha_I0 = -1.0 / 3.0 // -0.3333 -- 0.0 
    let beta_I0 = (1.0 - alpha_I0)**2.0 / 4.0 //  0.4444 -- 0.25
    let gamma_I0 = (1.0 - 2.0 * alpha_I0) / 2.0 //  0.8333 -- 0.5
    // Return record of type "input"
    {
      /// List of elements
      es =
        let initializer i = {material = (if i < numElem / 2 then m1 else m2); A = 1.0; L = 1.0; cn = [i; i + 1]}
        //let initializer i = {material = m2; A = 1.0; L = 1.0; cn = [i; i + 1]}
        List.init numElem initializer
      /// Prescribed accelerogram time step
      accelerogramTimeStep = 0.01
      /// Prescribed accelerogram data
      accelerogram = [0.0; 1.0; -2.0; 1.0; 0.0] @ List.init 10 (fun _ -> 0.0)
      /// Solution parameters
      sp =
        // Alpha = -1/3 works better than 0.
        let alpha = -0.1 // Parameter alpha belongs to <-1/3, 0>
        {
          alpha = alpha
          beta = (1.0 - alpha)**2.0 / 4.0
          gamma = (1.0 - 2.0 * alpha) / 2.0
          dt = 0.0001
        }
      alpha_I0 = alpha_I0
      beta_I0 = alpha_I0
      gamma_I0 = alpha_I0
    } // End of input

  /// Maximum recomanded time step.
  /// Formula taken from Hughes's book for linear 1d rod element with full mass matrix.
  let critDT =
    let get_dt_crit e = e.L / sqrt(3.0) / sqrt(e.material.E / e.material.rho)
    input.es |> List.map get_dt_crit |> List.min

  /// Run the analysis for given input
  let output = solve input



module Program = 
  [<EntryPoint>]
  //[<EntryPoint;STAThread>] // STAThread attribute is needed to invoke charts (FSharp.Charting).
  let main argv =
    printfn "Output is %A" Earthquake.output
    
    System.Console.ReadLine() |> ignore // Wait for key
    0 // return an integer exit code
