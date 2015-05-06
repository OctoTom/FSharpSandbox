#load @"c:\Users\Tomas\Scripts\NuGet\LoadPackages.fsx"

open MathNet.Numerics.LinearAlgebra
let rnd = System.Random()

// Correlation matrix
let num = 1000
//let init i j = if i = j then 1.0 else 0.7
let init i j =
  let isDiagonal = if i = j then 100.0 else 0.0
  rnd.NextDouble() + isDiagonal

let corrMat = Matrix.Build.Dense(num, num, init)

// Lower and upper triangular
let LT = L.Transpose()

let test = L * LT

let getRndVec () = Vector.Build.Dense(2, fun _ -> rnd.NextDouble() - 0.5)

// Vector of independent (uncorrelated) random numbers
let X = getRndVec ()

// Vector of correlated random numbers
let getCorrVec (X:Vector<float>) = L * X

//
let num = 100000
let uncorrelatedPairs = List.init num (fun _ -> getRndVec())
let correlatedPairs = uncorrelatedPairs |> List.map getCorrVec

let E (xi:float list) = xi |> List.average
let Var (xi:float list) =
  let mu = E xi
  xi |> List.averageBy (fun x -> System.Math.Pow(x - mu, 2.0))

let tempList = uncorrelatedPairs |> List.map (fun vec -> vec.[0])
let mu = E tempList
let var = Var tempList
let varTheoretical = 1.0 / 12.0

// Returns covariance matrix of given data
let getCovMatrix (data:Vector<float> list) =
  let numVar = data.Head.Count
  // http://en.wikipedia.org/wiki/Covariance#Definition 
  let cov (data:Vector<float> list) i j =
    let xList = data |> List.map (fun vec -> vec.[i])
    let yList = data |> List.map (fun vec -> vec.[j])
    let xyList = data |> List.map (fun vec -> vec.[i] * vec.[j])
    (xyList |> List.average) - (xList |> List.average) * (yList |> List.average)
  Matrix.Build.Dense(numVar, numVar, cov data)

// Retruns correlation matric of given data 
// http://en.wikipedia.org/wiki/Covariance_matrix#Correlation_matrix
let getCorrMatrix (data:Vector<float> list) =
  let numVar = data.Head.Count
  let covMat = getCovMatrix data
  let diagVec = covMat.Diagonal().Map(fun x -> 1.0 / System.Math.Sqrt(x))
  let diagMat = Matrix.Build.Diagonal(numVar, numVar, fun i -> diagVec.[i])
  diagMat * covMat * diagMat

let covMat_test = getCovMatrix correlatedPairs
let corrMat_test = getCorrMatrix correlatedPairs
let mu_1 = correlatedPairs |> List.averageBy (fun vec -> vec.[0])
let mu_2 = correlatedPairs |> List.averageBy (fun vec -> vec.[1])
