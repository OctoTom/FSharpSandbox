#r @"c:\Users\Tomas\Sync\Timber\LepenyNosnik\packages\MathNet.Numerics.3.11.0\lib\net40\MathNet.Numerics.dll"
#r @"c:\Users\Tomas\Sync\Timber\LepenyNosnik\packages\MathNet.Numerics.FSharp.3.11.0\lib\net40\MathNet.Numerics.FSharp.dll"
#load @"c:\Users\Tomas\Scripts\Paket\LoadPackages.fsx"

open System
open System.Runtime.InteropServices
open System.IO
open System.Numerics // System.Numerics.Complex
open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra

/// Three layer element
let nDOFbeam = 6
let nDOFsuper = 3 * nDOFbeam
let nDOFreduced = 10

/// Creates element stiffness matrix.
let getKe E G A As I L =
  let matrix = MathNet.Numerics.LinearAlgebra.Matrix.Build.Dense(nDOFbeam, nDOFbeam)
  matrix.[0,0] <- E * A / L
  matrix.[0,3] <- -E * A / L
  matrix.[3,0] <- -E * A / L
  matrix.[3,3] <- E * A / L
  //
  matrix.[1,1] <- G * As / L
  matrix.[1,4] <- -G * As / L
  matrix.[4,1] <- -G * As / L
  matrix.[4,4] <- G * As / L
  //
  matrix.[1,2] <- -G * As / 2.0
  matrix.[1,5] <- -G * As / 2.0
  matrix.[4,2] <- G * As / 2.0
  matrix.[4,5] <- G * As / 2.0
  //
  matrix.[2,1] <- -G * As / 2.0
  matrix.[2,4] <- G * As / 2.0
  matrix.[5,1] <- -G * As / 2.0
  matrix.[5,4] <- G * As / 2.0
  //
  matrix.[2,2] <- G * As * L / 4.0 + E * I / L
  matrix.[2,5] <- G * As * L / 4.0 - E * I / L
  matrix.[5,2] <- G * As * L / 4.0 - E * I / L
  matrix.[5,5] <- G * As * L / 4.0 + E * I / L
  // Return
  matrix

/// Returns stiffness matrix for given nu. Shear modulus G is assumed to be 1.0.
/// Multiply this matrix by the value of G and you will get the element stiffness matrix.
let getKeNormalized nu A As I L =
  let G = 1.0
  let E = 2.0 * G * (1.0 + nu)
  getKe E G A As I L

/// Creates element mass matrix.
let getMe rho A I L =
  let m, n = nDOFbeam, nDOFbeam
  let matrix = MathNet.Numerics.LinearAlgebra.Matrix.Build.Dense(m, n)
  let a, i = rho * A * L / 6.0, rho * I * L / 6.0
  matrix.[0,0] <- 2.0 * a
  matrix.[0,3] <- a
  matrix.[3,0] <- a
  matrix.[3,3] <- 2.0 * a
  //
  matrix.[1,1] <- 2.0 * a
  matrix.[1,4] <- a
  matrix.[4,1] <- a
  matrix.[4,4] <- 2.0 * a
  //
  matrix.[2,2] <- 2.0 * i
  matrix.[2,5] <- i
  matrix.[5,2] <- i
  matrix.[5,5] <- 2.0 * i
  // Return
  matrix

/// Localizes beam element matrices to a super element.  
let getSuper (a : Matrix<'T> list) =
  if a.Length <> 3 then failwith "Function getSuper() localized just three matrices."
  let m, n = nDOFsuper, nDOFsuper
  let matrix = MathNet.Numerics.LinearAlgebra.Matrix.Build.Dense(m, n)
  for i in [0..5] do
    for j in [0..5] do
      matrix.[i,j]       <- a.[0].[i,j]
      matrix.[i+6,j+6]   <- a.[1].[i,j]
      matrix.[i+12,j+12] <- a.[2].[i,j]
  // Return
  matrix

/// Creates transformation matrix taken from Alenka's hand-written notes.
let getTMatrix h1 h2 h3 = 
  let m, n = nDOFsuper, nDOFreduced
  let matrix = MathNet.Numerics.LinearAlgebra.Matrix.Build.Dense(m, n)
  matrix.[0,0] <- 1.0
  matrix.[1,1] <- 1.0
  matrix.[2,2] <- 1.0
  //
  matrix.[3,5] <- 1.0
  matrix.[4,6] <- 1.0
  matrix.[5,7] <- 1.0
  //
  matrix.[6,0] <- 0.5
  matrix.[6,2] <- h1 / 4.0
  matrix.[6,3] <- 0.5
  matrix.[6,4] <- -h3 / 4.0
  matrix.[7,1] <- 1.0
  matrix.[8,0] <- -1.0 / h2
  matrix.[8,2] <- -h1 / 2.0 / h2 
  matrix.[8,3] <- 1.0 / h2
  matrix.[8,4] <- -h3 / 2.0 / h2
  //
  matrix.[9,5] <- 0.5
  matrix.[9,7] <- h1 / 4.0
  matrix.[9,8] <- 0.5
  matrix.[9,9] <- -h3 / 4.0
  matrix.[10,6] <- 1.0
  matrix.[11,5] <- -1.0 / h2
  matrix.[11,7] <- -h1 / 2.0 / h2 
  matrix.[11,8] <- 1.0 / h2
  matrix.[11,9] <- -h3 / 2.0 / h2
  //
  matrix.[12,3] <- 1.0
  matrix.[13,1] <- 1.0
  matrix.[14,4] <- 1.0
  //
  matrix.[15,8] <- 1.0
  matrix.[16,6] <- 1.0
  matrix.[17,9] <- 1.0
  // Return
  matrix

