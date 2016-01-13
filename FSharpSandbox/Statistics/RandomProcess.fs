//#load @"c:\Users\Tomas\Scripts\NuGet\LoadPackages.fsx"
module RandomProcess
open MathNet.Numerics.LinearAlgebra

// -- Auxiliary stuff ---------------------------------------------------------
/// Takes two list and creates list using custom mapping
/// productMap [1..3] [11..14] (*) --> [[11; 12; 13; 14]; [22; 24; 26; 28]; [33; 36; 39; 42]]
let productMap xList yList mapping =
  let outerMapping xItem = yList |> List.map (mapping xItem)
  xList |> List.map outerMapping
// Test: productMap [1..3] [11..14] (*)

/// Converts list of lists into matrix. Lists become rows.
let listsToMatrix (lists:float list list) =
  let m = lists.Length
  let n = lists.Head.Length
  let matrix = Matrix.Build.Dense(m, n)
  let action i list = list |> List.iteri (fun j value -> matrix.[i, j] <- value)
  lists |> List.iteri action
  matrix
// ----------------------------------------------------------------------------

// Covariance function.
// Process variance (var), correlation length (theta) and distance (h).
let covFun var hc h = var * System.Math.Exp(-2.0 * h / hc)

// ====================================================================
// == Gaussian Markov Random Field (GMRF) generation                 
// Written according to "Spatial Process Generation.pdf", Algorithm 2.1
let getRandomField (dx1, dx2) (n1, n2) covFunc =
  // First get some points. For simplicity suppose points in grid.
  let deltaX1 = dx1 // [m]
  let deltaX2 = dx2 // [m]

  let numPoint1 = n1
  let numPoint2 = n2

  // Index i represent row
  // Index j represent column
  let positions =
    [for i in 0..numPoint1-1 do
     for j in 0..numPoint2-1 do
     yield (i, j)]

  let points =
    [for i in 0..numPoint1-1 do
     for j in 0..numPoint2-1 do
     yield Vector.Build.DenseOfArray([float i * deltaX1; float j * deltaX2] |> List.toArray)]

  // Number of points denoted as n
  let n = points.Length

  // Covariance for given two points
  let getCov (x1:Vector<float>) (x2:Vector<float>) =
    // printf "Function getCov() started.\n"
    let diff = x1 - x2 // Vector of distance
    let h = diff.L2Norm() // Distance (scalar)
    covFunc h

  // Covariance matrix
  let Sigma =
    let temp = productMap points points getCov
    let sigmaMat = temp |> listsToMatrix
    do printf "Size of matrix Sigma is: %A x %A.\n" sigmaMat.RowCount sigmaMat.ColumnCount
    sigmaMat

  // Lower and upper triangular
  let L =
    do printf "Cholesky decomposition started.\n"
    let res = Sigma.Cholesky().Factor
    do printf "Cholesky decomposition completed.\n"
    res

  let Z =
    let rnd = System.Random(0);
    let arr = MathNet.Numerics.Distributions.Normal.Samples(rnd, 0.0, 1.0) |> Seq.take(n) |> Seq.toArray
    Vector.Build.DenseOfArray(arr)

  // Output - values at points i = 1 .. n
  let output = L * Z
  
  let gridValues = 
    let list = output |> Seq.toList
    let result = Array2D.create numPoint1 numPoint2 0.0
    let action i =
      let x, y = positions.[i]
      result.[x, y] <- list.[i]
    [0..n-1] |> List.iter  action  
    result
  
  // Return expression
  (points, gridValues)

//////////////////////////////////////////
// let myArray = Array2D.init 1000 500 (fun x y -> 0.05 * float x * float y)

let saveGridValuesAsBitmap fileName gridValues =
  let getMinMax (array:float[,]) =
    let width = array.GetLength 0
    let height = array.GetLength 1
    let mutable min = System.Double.MaxValue
    let mutable max = System.Double.MinValue
    for x in 0..width-1 do
      for y in 0..height-1 do
        let item = array.[x, y]
        if item > max then max <- item
        if item < min then min <- item
    (min, max)
  
  let normalize (array:float[,]) =
    let min, max = getMinMax array
    let normValue x = 255.0 * (x - min) / (max - min) |> int
    let initizer x y = array.[x, y] |> normValue
    Array2D.init (array.GetLength 0) (array.GetLength 1) initizer

  let saveFieldToBitmap fileName (field:int[,]) =
    let width = field.GetLength 0
    let height = field.GetLength 1
    let bitmap = new System.Drawing.Bitmap(width, height)
    let scalarToColor (x:int) = System.Drawing.Color.FromArgb(x, x, x)
    for x in 0..bitmap.Width-1 do
      for y in 0..bitmap.Height-1 do
        bitmap.SetPixel(x, y, field.[x, y] |> scalarToColor)
    bitmap.Save(fileName)
  
  // Return expression
  do gridValues |> normalize |> saveFieldToBitmap fileName


