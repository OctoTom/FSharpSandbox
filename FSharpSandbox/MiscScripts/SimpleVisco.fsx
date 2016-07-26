#r @"c:\Users\Tomas\Sync\Timber\LepenyNosnik\packages\MathNet.Numerics.3.11.0\lib\net40\MathNet.Numerics.dll"
#r @"c:\Users\Tomas\Sync\Timber\LepenyNosnik\packages\MathNet.Numerics.FSharp.3.11.0\lib\net40\MathNet.Numerics.FSharp.dll"

open System
open System.Runtime.InteropServices
open System.IO
open System.Numerics // System.Numerics.Complex
open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra

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

// Visco-elastic dynamics problem that we are solving with Alenka, Honza and Jarda. //
let n = 3
let m1 = 0.5
let m2 = 0.8
let m3 = 0.6
let M = DiagonalMatrix.ofDiagArray [|m1; m2; m3|]
// Stiffness matrix independent on frequency
let k1 = 4.3
let k3 = 5.1
// k2 is composed of spring parallel to spring chaninded to dumper. 
let kinf = 0.5
let ki = 1.8
let ti = 5.0
// Auxiliary controll parameter
let epsilon = 1.0

// Helper functions and values
let c a = System.Numerics.Complex.Create(a, 0.0)
let i = Complex.ImaginaryOne
let c0 = Complex.Zero

// Real-valued stiffness matrix independent from omega
let K0 =
  DenseMatrix.ofRowList [[k1; -k1; 0.0]; [-k1; k1; 0.0]; [0.0; 0.0; k3]] +
  DenseMatrix.ofRowList [[0.0; 0.0; 0.0]; [0.0; kinf; -kinf]; [0.0; -kinf; kinf]]
// Complex-valued omega-dependent part of stiffness matrix
let K1 (omega: Complex) =
  let ti = c ti
  let kCplx = (c ki) * (omega * omega * ti * ti + i * omega * ti) / (c 1.0 + omega * omega * ti * ti)
  DenseMatrix.ofRowList [[c0; c0; c0]; [c0; kCplx; -kCplx]; [c0; -kCplx; kCplx]]
// Complete lhs of K(omega) - omega^2 * M = 0
let Ktot omega =
  K0.Map (fun x -> c x) + c epsilon * K1 omega - omega * omega * M.Map (fun x -> c x)

// Compute initial (purely elastic) solution
// and select one eigenvalue and one eigenvector.
let evd = (M.Inverse() * K0).Evd(Symmetricity.Asymmetric)
let chosenIndex = 0
let omega0 = sqrt (evd.EigenValues.At(chosenIndex).Real)
let r0 = evd.EigenVectors.Column(chosenIndex)

// Initial state - my values or values from SimpleVicso.m
let x0 = DenseVector.ofList [omega0; r0.[0]; r0.[1]; r0.[2]]
// The same values from Matlab. The eigen shape is normed differently.
//let x0 = DenseVector.ofList [0.5861; -0.8973; -0.8614; -0.0799]

// Function to be zeroed.
let f (x : Vector<Complex>) =
  if x.Count <> 4 then failwith "Dimension has to be 4."
  let omega, r = x.[0], x.SubVector(1, 3)
  let vec = Ktot omega * r
  let scalar = r0.Map (fun x -> c x) * (r - r0.Map (fun x -> c x))
  let res = DenseVector.ofList [scalar; vec.[0]; vec.[1]; vec.[2]]
  printfn "f(x) = %A" res
  res

// Derivatives of f(x).
let J (x : Vector<Complex>) =
  let omega, r = x.[0], x.SubVector(1, 3)
  let Ktot = Ktot omega
  let dK = -i * c ki * c ti / ((c ti * omega - i) * (c ti * omega - i))
  let dKmatrix = DenseMatrix.ofRowList [[c0; c0; c0]; [c0; dK; -dK]; [c0; -dK; dK]]
  let vec = (c epsilon * dKmatrix - c 2.0 * omega * M.Map (fun x -> c x)) * r
  let list =
    [
      [c 0.0; c r0.[0]; c r0.[1]; c r0.[2]];
      [vec.[0]; Ktot.[0,0]; Ktot.[0,1]; Ktot.[0,2]];
      [vec.[1]; Ktot.[1,0]; Ktot.[1,1]; Ktot.[1,2]];
      [vec.[2]; Ktot.[2,0]; Ktot.[2,1]; Ktot.[2,2]];
    ]
  let res = DenseMatrix.ofRowList list
  printfn "J(x) = %A" res
  res
   
let sqn = newtonsMethod 1.0e-5 f J (x0.Map (fun x -> c x))
sqn |> Seq.length
sqn |> Seq.toList

let norms = sqn |> Seq.toList |> List.map (fun item -> item.norm)

/// Ratios between current and preceding norm.
let ratios = 
  let norms1 = norms |> Seq.tail
  let norms2 = norms |> Seq.rev |> Seq.tail |> Seq.rev
  Seq.map2 (fun a b -> a / b) norms1 norms2 |> Seq.toList


