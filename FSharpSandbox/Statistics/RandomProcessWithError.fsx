(*
===========================================================
This script tests numerically the following:

if
xVec ~ MultivarNormal(muVec, covMat)
y_i ~ Norm(xVec_i, var_i)

then
yVec ~ MultivarNormal(muVec, covMat + diag(varVec))
===========================================================
*)



// 1. Load references to F# Interactive!
#load @"c:\Users\Tomas\Scripts\NuGet\LoadPackages.fsx"

let rnd = System.Random(0);

let lolToMatrix (lol : double list list) =
  MathNet.Numerics.LinearAlgebra.DenseMatrix.init lol.Length lol.Head.Length (fun i j -> lol.[i].[j]) 

let nDim = 2
let mu = [[100.0]; [110.0]] |> lolToMatrix
let cov = [[1.2; 0.4]; [0.4; 0.9]] |> lolToMatrix

// Lower and upper triangular
let L =
  do printf "Cholesky decomposition started.\n"
  let res = cov.Cholesky().Factor
  do printf "Cholesky decomposition completed.\n"
  res

let getCorrSample () =
  let Z =
    let arr = MathNet.Numerics.Distributions.Normal.Samples(rnd, 0.0, 1.0) |> Seq.take(nDim) |> Seq.toList |> List.map (fun x -> [x])
    arr |> lolToMatrix
  let X = mu + L * Z
  X

let corrSamples = Seq.initInfinite (fun _ -> getCorrSample())

let nSamples = 500000
let samplesMat = corrSamples |> Seq.take nSamples |> Seq.toList

// List of correlated samples.
let getMuCov (samples : double list list) =
  let nSamples = samples.Length
  let getXs iDim = samples |> List.map (fun x -> x.[iDim])
  let getAverage iDim = getXs iDim |> List.average
  let getCovariance iDim jDim =
    let xi = getXs iDim
    let xj = getXs jDim
    let xixj = List.map2 (*) xi xj
    List.average xixj - List.average xi * List.average xj
  let averages = [0..nDim-1] |> List.map getAverage
  let covariances = [0..nDim-1] |> List.map (fun i -> [0..nDim-1] |> List.map (fun j -> getCovariance i j))
  (averages, covariances)

let samples = samplesMat |> List.map (fun mat -> List.init nDim (fun i -> mat.[i,0]))
let samplesWithError =
  let variances = [1.0; 2.0]
  let sigmas = variances |> List.map sqrt
  //let mapping (sample : double list) = sample |> List.mapi (fun i x -> MathNet.Numerics.Distributions.Normal.Sample(rnd, x, sigmas.[i]))
  let mapping (sample : double list) = sample |> List.mapi (fun i x -> x + MathNet.Numerics.Distributions.Normal.Sample(rnd, 0.0, sigmas.[i]))
  samples |> List.map mapping

samples |> getMuCov
samplesWithError |> getMuCov



