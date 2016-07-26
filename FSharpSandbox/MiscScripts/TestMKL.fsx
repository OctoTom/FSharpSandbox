#r @"c:\Users\Tomas\Sync\Timber\LepenyNosnik\packages\MathNet.Numerics.3.11.0\lib\net40\MathNet.Numerics.dll"
#r @"c:\Users\Tomas\Sync\Timber\LepenyNosnik\packages\MathNet.Numerics.FSharp.3.11.0\lib\net40\MathNet.Numerics.FSharp.dll"

open System
open System.Runtime.InteropServices
open System.IO
open System.Numerics // System.Numerics.Complex
open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra

Control.UseNativeMKL()

let p = MathNet.Numerics.Providers.LinearAlgebra.Mkl.MklLinearAlgebraProvider()
let cplx (a, b) = System.Numerics.Complex(a, b)
let rtc a = cplx (a, 0.0)
let cplx0 = cplx(0.0, 0.0)
let zeroArr n = List.init n (fun _ -> cplx0) |> List.toArray

// Wikipedia example
// https://en.wikipedia.org/wiki/Eigenvalues_and_eigenvectors#Two-dimensional_matrix_example
let n = 2
let A = [rtc 2.0; rtc 1.0; rtc 1.0; rtc 2.0] |> List.toArray
let lambdas = zeroArr n
let rs = zeroArr (n * n)
let matrixD = zeroArr (n * n)
let result = p.EigenDecomp(true, n, A, rs, lambdas, matrixD)

// Wikipedia example with complex eigenvalues
// https://en.wikipedia.org/wiki/Eigenvalues_and_eigenvectors#Three_dimensional_matrix_example_with_complex_eigenvalues
let n = 3
let A = [rtc 0.0; rtc 1.0; rtc 0.0; rtc 0.0; rtc 0.0; rtc 1.0; rtc 1.0; rtc 0.0; rtc 0.0] |> List.toArray
let lambdas = zeroArr n
let rs = zeroArr (n * n)
let matrixD = zeroArr (n * n)
do p.EigenDecomp(false, n, A, rs, lambdas, matrixD)





