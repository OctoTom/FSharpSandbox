module OptimModule

let aa = MathNet.Numerics.LinearAlgebra.Double.DenseVector.ofList

// Computes sum of squares of differences in values of two lists.
let sumOfSquares = List.fold2 (fun acc x y -> acc + (x - y) * (x - y)) 0.0

// Fits given real value inside an interval.
let fitToInterval (min, max) x =
  match x with
  | x when x < min -> min
  | x when x > max -> max
  | _ -> x
// Fits a given point inside prescribed bounds.
let fitToBounds bounds x =
  List.map2 fitToInterval bounds x
// Get the middle point of interval
let middleOfInterval (min, max) = (min + max) / 2.0
let middleOfBounds bounds = List.map middleOfInterval bounds


/// .NET Random number generator
let random = new System.Random()
/// Infinite sequence of uniformly distributed random n-dim points in interval (0.0, 1.0).
//let samplesUD n = Seq.unfold (fun () -> Some(List.init n (fun i -> random.NextDouble()), ())) ()
let samplesUD n = Seq.initInfinite (fun i -> List.init n (fun i -> random.NextDouble()))

let samplesUD_Interval (bounds:(float*float) list) =
  let n = bounds.Length
  let matToInterval bounds x = fst bounds + (snd bounds - fst bounds) * x // Projects from (0.0, 1.0) to (low, upp)
  (samplesUD n) |> Seq.map (fun x_ud -> x_ud |> List.map2 matToInterval bounds) 

let optimize_bestRandom objFunc bounds n =
  let samples = samplesUD_Interval bounds |> Seq.take n
  let realizations = samples |> Seq.map (fun x -> (x, objFunc x))
  realizations |> Seq.minBy snd

// Operator computing derivation of function numerically.
let numDiff dx f =
  fun x -> (f (x + dx) - f (x - dx)) / 2.0 / dx
// Instance of operator with prescribed perturbation size.
let d = numDiff 1e-6

// Returns list of functions representing gradients of a given function f.
let numGrad dx n f =
  let pert_ith (dx:float) i xs =
    xs |> List.mapi (fun j x -> if i = j then x + dx else x)
  /// Function that creates perturbated xs
  let pert (dx:float) xs =
    xs |> List.mapi (fun i _ -> (pert_ith dx i xs))
  // Return expression 
  List.init n (fun i -> (fun xs ->
    let xp = pert_ith dx i xs
    (f xp - f xs) / dx))

let getGradAtPoint dx f (x:float list) =
  let n = x.Length
  (numGrad dx n f) |> List.map (fun df -> df x)

let numHessianOp dx n f =
  let grad = numGrad dx n
  f |> grad |> List.map grad

let getHessianAtPoint dx f (x:float list) =
  let n = x.Length
  (numHessianOp dx n f) |> List.map (List.map (fun ddf -> ddf x))

let getNewPoint dx f (x:float list) (mult:float) =
  let n = x.Length
  // Compute inverted Hessian matrix
  let hessian = getHessianAtPoint dx f x
  let hessMatrix = MathNet.Numerics.LinearAlgebra.Double.DenseMatrix.OfArray(array2D hessian)
  let hessMatrix_inv = hessMatrix.Inverse()
  // Compute gradient column vector
  let grad = getGradAtPoint dx f x
  let gradVector = MathNet.Numerics.LinearAlgebra.Double.DenseVector.ofList(grad)
  let oldPoint = MathNet.Numerics.LinearAlgebra.Double.DenseVector.ofList(x)
  let newPoint = oldPoint - mult * hessMatrix_inv * gradVector
  List.ofArray (newPoint.ToArray()) 

let newtonOptimize dx f (x:float list) norm maxN bounds mult=
  let rec newtonHelper n x =
    let xOld = x
    // Fit the new value onto bounds
    let x = (getNewPoint dx f x mult) |> fitToBounds bounds // Shadow the old value of x
    match x, n with
    | x, n when n < maxN -> newtonHelper (n + 1) x
    | x, n -> x
    | x, _ when ((List.zip x xOld) |> List.sumBy (fun x -> ((fst x) - (snd x))**2.0))**0.5 < norm  -> x
    | _ -> failwith ("To many steps in Newton's optimalization: " + n.ToString() + " " + x.ToString())
  newtonHelper 0 x

// TESTS // 
let someFunc (x:float list) =
  let x1, x2 = x.[0], x.[1]
  2.0 * x1**2.0 + x2**2.0 + x1 - 3.0 * x2 + x1 * x2
let someBounds = [(-2.0, 1.0); (-2.0, 1.0)]

let res = newtonOptimize 1e-6 someFunc [1.0; 1.0] 1.0 1 someBounds 1.0





