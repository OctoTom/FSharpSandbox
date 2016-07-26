#load @"c:\Users\Tomas\Scripts\Paket\LoadPackages.fsx"
#r @"c:\Users\Tomas\OneDrive\OneSync\Projects\CommonTools\CommonTools\bin\Release\CommonTools.dll"
#r @"c:\Users\Tomas\OneDrive\OneSync\Projects\FSharpSandbox\FSharpSandbox\packages\MathNet.Numerics.MKL.Win.2.0.0\build\x64\MathNet.Numerics.MKL.dll"
open MathNet.Numerics.LinearAlgebra
open CommonTools
open FSharp.Charting

// Put this to Wolfram Alpha and consider that (x = omega*omega)
// (k-x*m)*(2*k-x*m)*(2*k-x*m)-k*k*(k-x*m)-k*k*(2*k-x*m)=0
// Express angular frequecy as omega_i = sqrt(a_i * k / m)

let a = [0.1981; 1.1555; 3.247]
let r a = [1.0; 1.0 - a; (2.0 - a) * (1.0 - a) - 1.0]
let rs = a |> List.map r
let shapesToPlot = rs |> List.map (fun r -> r @ [0.0])
let iterator i =
  let name = "Shape " + string (i + 1)
  let fileName = "c:/Users/Tomas/Sync/" + name + ".png"
  shapesToPlot.[i] |> Chart.Line |> Chart.WithTitle name |> Chart.Save fileName
[0..2] |> List.iter iterator

let complexMatrix = MathNet.Numerics.LinearAlgebra.Complex.DenseMatrix(2)
complexMatrix.[0,0] <- System.Numerics.Complex(1.0, 2.0)
complexMatrix.[0,1] <- System.Numerics.Complex(1.0, 0.0)
complexMatrix.[1,0] <- System.Numerics.Complex(1.0, 3.0)
complexMatrix.[1,1] <- System.Numerics.Complex(3.0, 2.0)

let aaa = new MathNet.Numerics.LinearAlgebra.Factorization.Evd<System.Numerics.Complex>()

let p = MathNet.Numerics.Providers.LinearAlgebra.Mkl.MklLinearAlgebraProvider()
let cplx (a, b) = System.Numerics.Complex(a, b)
let cplx0 = cplx(0.0, 0.0)
let order = 2
let matrix = [cplx(1.0, 2.0); cplx(2.0, 0.0); cplx(1.0, 1.0); cplx(3.0, 2.0)] |> List.toArray
let eigenVecs = List.init (order * order) (fun _ -> cplx0) |> List.toArray
let eigenVals = List.init (order) (fun _ -> cplx0) |> List.toArray
let matrixD = List.init (order * order) (fun _ -> cplx0) |> List.toArray
let result = p.EigenDecomp(true, order, matrix, eigenVecs, eigenVals, matrixD)
