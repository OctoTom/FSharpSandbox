#r @"c:\Users\Tomas\Sync\Timber\LepenyNosnik\packages\MathNet.Numerics.3.11.0\lib\net40\MathNet.Numerics.dll"
#r @"c:\Users\Tomas\Sync\Timber\LepenyNosnik\packages\MathNet.Numerics.FSharp.3.11.0\lib\net40\MathNet.Numerics.FSharp.dll"
#load @"c:\Users\Tomas\Scripts\Paket\LoadPackages.fsx"
#load @"TriLayerElement.fsx" // Module TLE

open System
open System.Runtime.InteropServices
open System.IO
open System.Numerics // System.Numerics.Complex
open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra
open TriLayerElement


// Complex Newton's method.
type State = {x: Vector<Complex>; f: Vector<Complex>; norm: float}
/// Newton's method for vector-valued complex function.
/// Returns sequence of states.
let newtonsMethod tol (f: Vector<Complex> -> Vector<Complex>) (J: Vector<Complex> -> Matrix<Complex>) x0 =
  let getState x =
    let fx = f x 
    let norm = fx.L2Norm()
    {x = x; f = fx; norm = norm}
  let generator (state: State) =
    if state.norm < tol then
      None
    else
      let Jx = J state.x
      let Jinv = Jx.Inverse()
      let xNew = state.x - Jinv * state.f
      let stateNew = getState xNew
      Some(stateNew, stateNew)
  let state0 = getState x0
  let sqn = Seq.unfold generator state0
  Seq.append (Seq.singleton state0) sqn
// TEST  
//let tol = 1.0e-5
//let f (x : Vector<Complex>) = x
//let J (x : Vector<Complex>) =
//  let array = [|Complex.Create(1.1, 0.0); Complex.Create(0.9, 0.0)|] // Derivatives are a bit wrong.
//  Complex.DenseMatrix.OfDiagonalArray array :> Matrix<Complex>
//let x0 = DenseVector.ofList [Complex.Create(1.0, 0.0); Complex.Create(1.0, 0.0)]
//let sqn = newtonsMethod tol f J x0 |> Seq.toList



// Model parameters
let beamLengh = 1.0 // m
let nElem = 10

// Parameters common to one element
let l = beamLengh / float nElem // m
let b = 1.0 // m (out of plane width)
/// Parameters charactaristic to a single layer
type LayerParams = {G : float; nu : float; rho : float; h : float }
let layer1 = {G = 26.1e6; nu = 0.0; rho = 2600.0; h = 0.005}
let layer2 = {G = 26.1e6; nu = 0.0; rho = 2600.0; h = 0.005}
//let layer2 = {G = 4.1e6; nu = 0.0; rho = 1600.0; h = 0.001}
/// List of layers. Third layer is idnetical to first. The cross section is symmetric.
let layers = [layer1; layer2; layer1]

/// Returns 6x6 element stiffness matrix of single layer.
let getKForLayer l (layer:LayerParams) =
  let A = b * layer.h
  let As = 5.0 / 6.0 * A
  let I = 1.0 / 12.0 * b * layer.h**3.0
  layer.G * getKeNormalized layer.nu A As I l

/// Returns 6x6 element mass matrix of single layer.
let getMForLayer l (layer:LayerParams) =
  let A = b * layer.h
  let I = 1.0 / 12.0 * b * layer.h**3.0
  getMe layer.rho A I l

/// Transformation matrix that transforms 18x18 super matrix to 10x10 reduced matrix.
let T = getTMatrix layers.[0].h layers.[1].h layers.[2].h

let Ksuper = layers |> List.map (getKForLayer l) |> getSuper
let Kreduced = T.Transpose() * Ksuper * T
// Kreduced.IsSymmetric()

let Msuper = layers |> List.map (getMForLayer l)|> getSuper
let Mreduced = T.Transpose() * Msuper * T



// LOCALIZATION
// Node numbers [0 .. nElem]
// Element numbers [0 .. nElem - 1]

/// Named master DOFs on one node.
/// "T" denotes top layer, "B" denotes bottom layer.

let getDOFPosition name =
  let namedNodeDOF = ["uT"; "wT"; "phiT"; "uB"; "phiB"]
  namedNodeDOF |> List.findIndex ((=) name)  

/// Returns list of code numbers for given node.
/// Nodes are numbered from 0 to nElem. 
/// Code numbers are options. None means that the DOF is supported. 
let getNodeCN_v1 iNode =
  if iNode < 0 then failwith "Negative node number."
  if iNode > nElem then failwith "Node number is higher then number of nodes."
  match iNode with
  | 0 -> [Some 0; None; Some 1; None; Some 2] // First node
  | i when i = nElem -> [Some(i*5-2); None; Some(i*5-1); Some(i*5); Some(i*5+1)] // Last node
  | i -> [i*5-2 .. i*5+2] |> List.map Some // Nodes in between
let getNodeCN_v2 iNode =
  if iNode < 0 then failwith "Negative node number."
  if iNode > nElem then failwith "Node number is higher then number of nodes."
  match iNode with
  | 0 -> [None; None; Some 0; None; Some 1] // First node
  | i when i = nElem -> [Some(i*5-3); None; Some(i*5-2); Some(i*5-1); Some(i*5)] // Last node
  | i -> [i*5-3 .. i*5+1] |> List.map Some // Nodes in between
let getNodeCN = getNodeCN_v1
// Test
// [0..nElem] |> List.map getNodeCN


/// Returns list of code numbers for given element.
/// Elements are numbered from 0 to nElem-1 
let getElemCN iElem =
  let nodeNumbers = [iElem; iElem+1]
  nodeNumbers |> List.collect getNodeCN
// Test
// getElemCN 1

/// Number of DOFs, number of Some code numbers.
let nDOF = [0..nElem] |> List.collect getNodeCN |> List.choose id |> List.length

let Localize (elemMatrix : MathNet.Numerics.LinearAlgebra.Matrix<float>) =
  if elemMatrix.RowCount <> nDOFreduced || elemMatrix.ColumnCount <> nDOFreduced then failwith "Wrong dimension of element matrix."
  let matrix = MathNet.Numerics.LinearAlgebra.Matrix<float>.Build.Dense(nDOF, nDOF)
  let action iElem =
    let cn = getElemCN iElem
    for i in [0..9] do
      for j in [0..9] do
        match cn.[i], cn.[j] with
        | Some a, Some b -> matrix.[a, b] <- matrix.[a, b] + elemMatrix.[i, j]
        | _ -> ()
  [0 .. nElem-1] |> List.iter action
  matrix

// Assembly the global stiffness and mass matrices.
let Kglobal = Localize Kreduced
let Mglobal = Localize Mreduced

// Now find omega^2 for which
// det[K - omega^2 * M] = 0
let evd = (Kglobal * Mglobal.Inverse()).Evd(Symmetricity.Asymmetric) // Caution! (Kg * Mg^-1) is not symmetric.
let omegas, rs = 
  let omegasUnsort = evd.EigenValues |> Seq.toList |> List.map (fun c -> sqrt c.Real)
  let rsUnsort = evd.EigenVectors.EnumerateColumns() |> Seq.toList |> List.map Seq.toList
  let zipped = List.zip omegasUnsort rsUnsort |> List.sortBy fst
  zipped |> List.map fst, zipped |> List.map snd

let getQuantity (r : float list) name =
  let i = getDOFPosition name
  let codeNums = [0..nElem] |> List.map getNodeCN |> List.map (fun cns -> cns.[i])
  let mapping cn =
    match cn with
    | Some a -> r.[a]
    | None -> 0.0
  codeNums |> List.map mapping

let plotEigenshape i = getQuantity (rs.[i] |> Seq.toList) "wT" |> FSharp.Charting.Chart.Line
// Pring few of the firs eigenshapes
[0..3] |> List.map plotEigenshape

omegas |> Seq.head

// Simply supported beam first mode natural frequency
let omegaN =
  let E = layer1.G * 2.0 * (1.0 + layer1.nu)
  let h = layers |> List.sumBy (fun l -> l.h)
  let I = 1.0 / 12.0 * b * h**3.0
  let L = beamLengh
  let m = h * b * layer1.rho
  System.Math.PI**2.0 * sqrt (E * I / m / L**4.0)



// Visco-elastic part - only middle (second) layer
// -----------------------------------------------
let Gs = [10.0e6; 40.0e6]
let taus = [50.0; 200.0]  
let nu = layer2.nu
// "Load increment" in the N-R method. Complex part of the global stiffness matrix can be added incrementally.
let epsilon = 1.0

// Helper function. Creates Complex from float.
let c x = Complex(x, 0.0)
let i = Complex.ImaginaryOne
// Returns list of complex dynamic moduli for given omega
let getGdyns_cplx (omega : Complex) =
  // Returns storage modulus of Maxwell cell with given G, tau and omega.
  let getG' G tau omega = tau * tau * omega * omega / (c 1.0 + tau * tau * omega * omega) * G
  // Returns loss modulus of Maxwell cell with given G, tau and omega.
  let getG'' G tau omega = tau * omega / (c 1.0 + tau * tau * omega * omega) * G
  // Return
  List.map2 (fun G tau -> getG' G tau omega + Complex.ImaginaryOne * getG'' G tau omega) (Gs |> List.map c) (taus |> List.map c)

/// Retrns comlex 6x6 element stiffness matrix. Normalized matrix. It has to be multiplied by sumG.
let getKeNorm layer L=
  let A = b * layer.h
  let As = 5.0 / 6.0 * A
  let I = 1.0 / 12.0 * b * layer.h**3.0
  (getKeNormalized nu A As I L)

let sumG_cplx omega = omega |> getGdyns_cplx |> List.reduce (+)

let KeNorm = getKeNorm layer2 l
let Kdummy = MathNet.Numerics.LinearAlgebra.Matrix<float>.Build.Dense(6, 6)

// Complex super-element stiffness matrix.
let KsuperNorm = [Kdummy; KeNorm; Kdummy] |> getSuper
// Complex reduced stiffness matrix.
let KreducedNorm = T.Transpose() * Ksuper * T
// Kreduced_cplx (c 10.0)


/// Complex and omega-dependent part of the global stiffness matrix.
let KglobalNorm =
  Localize KreducedNorm




/// Total matrix that needs to yield zero vector when multiplied with r.
let totMatrix omega =
  Kglobal.Map (fun x -> c x) + c epsilon * sumG_cplx omega * KglobalNorm.Map(fun x -> c x) - omega * omega * Mglobal.Map (fun x -> c x)



// Initial state - my values or values from SimpleVicso.m
let chosenIndex = 0
let x0 =
  let omega = omegas.[chosenIndex]
  let r = rs.[chosenIndex]
  ((omega :: r) |> DenseVector.ofList).Map(fun x -> c x)
let r0 = x0.SubVector(1, nDOF)

// Function to be zeroed.
let f (x : Vector<Complex>) =
  if x.Count <> nDOF + 1 then failwith "Dimension has to be NumDof + 1."
  let omega, r = x.[0], x.SubVector(1, nDOF)
  let scalar = r0 * (r - r0)
  let vec = totMatrix omega * r
  let res = DenseVector.ofList (scalar :: (vec |> Seq.toList))
  //printfn "f(x) = %A" res
  res

// Derivatives of f(x).
let J (x : Vector<Complex>) =
  let omega, r = x.[0], x.SubVector(1, nDOF)
  let totMatrix = totMatrix omega
  let dG = List.map2 (fun G tau -> -i * c G * c tau / ((c tau * omega - i) * (c tau * omega - i))) Gs taus |> List.reduce (+)
  let vec = (c epsilon * dG * KglobalNorm.Map(fun x -> c x) - c 2.0 * omega * Mglobal.Map (fun x -> c x)) * r
  let list =
    (c 0.0 :: (r0 |> Seq.toList)) :: // First row
    List.init nDOF (fun i -> vec.[i] :: List.init nDOF (fun j -> totMatrix.[i,j])) // Rest of rows
  let res = DenseMatrix.ofRowList list
  //printfn "J(x) = %A" res
  res

let sqn = newtonsMethod 1.0e-5 f J x0
sqn |> Seq.take 30 |> Seq.map (fun item -> item.norm) |> Seq.toList

let state = sqn |> Seq.skip 29 |> Seq.head
let omega :: r = state.x |> Seq.toList

getQuantity (r |> List.map (fun x -> x.Real)) "wT" |> FSharp.Charting.Chart.Line


(*
// Input taken from the Alenka's matlab.

// Temperature 
//T = 30; // °C 20, 25, 30, 35, 40
let temperature = 30.0

// Geometry
let L = 1 // m
let B = 0.01 // m
let h = [7.9e-3; 0.38e-3; 3.75e-3] // m

// Glass
let E_g = 72.0e9 //  Pa
let nu_g = 0.22 // [-] 
let rho_g = 2500.0 // kg/m3

// PVB
let rho = 1060.0 // kg/m3

let C1 = 12.60
let C2 = 74.46;
let T0 = 20.0 // Celsius
let at = 10.0**(-C1 * (temperature - T0) / (C2 + temperature - T0))

//inp.K = 2*1e9; % Pa
let nu = 0.49 // [-]
let Ginf = 1.9453569e5; // Pa
let taup =
  [2.366E-07; 2.2643E-06; 2.16668E-05; 2.073273E-04; 1.9838958E-03;
  1.89837195E-02; 1.816534983E-01; 1.7382259321E+00; 1.66329270788E+01;
  1.591589781894E+02; 1.52297789909670E+03; 1.45732380763177E+04; 1.3945E+05] |> List.map ((*) at)
let Gp =
  [99482242.0; 90802159.0; 74139637.0; 57856234.0; 50771943.0; 29055494.0; 17601286.0;  3080246.5;
  1200106.0; 224051.46; 182366.18; 115227.71; 41645.31];
taup |> List.length
Gp |> List.length   

// Number of elements
let Ne = 100;

// Number of mode shapes
let n_modes = 6;


// RESULTS: Thesis Arnaud Jaubert version of 2 Feb 2016.pdf, Page 55 + 58

// Simply supported beam, page 59
// FIRST MODE
// Temp 30°C
// Experiment                 Finite element 
// Frequency (hz) Loss factor Frequency (hz) Loss factor Error frequency 
// 30.92          2.22        28.83          3.16        7.26

// SECOND MODE
// Temp 30°C
// Experiment                 Finite element 
// Frequency (hz) Loss factor Frequency (hz) Loss factor Error frequency 
// 116.05         2.46        112.84         4.84        2.84

// THIRD MODE
// Temp 30°C
// Experiment                 Finite element 
// Frequency (hz) Loss factor Frequency (hz) Loss factor Error frequency 
// 255.44         4.00        248.30         6.58        2.87

// Free-free beam, page 63
// FIRST MODE (NENI PRVNI!!!)
// Temp 30°C
// Experiment                 Finite element 
// Frequency (hz) Loss factor Frequency (hz) Loss factor Error frequency 
// 65.97          2.11        65.55          1.99        0.64

// SECOND MODE
// Temp 30°C
// Experiment                 Finite element 
// Frequency (hz) Loss factor Frequency (hz) Loss factor Error frequency 
// 179.30         3.75        176.60         4.51        1.53

// THIRD MODE
// Temp 30°C
// Experiment                 Finite element 
// Frequency (hz) Loss factor Frequency (hz) Loss factor Error frequency 
// 346.40         5.30        338.85         5.89        2.23
*)