// Sparse grid construction as defined in: T. Gerstner - Numerical integration using sparse grid
// Domain: Omega = [-1.0, 1.0]

#load @"c:\Users\Tomas\Scripts\NuGet\LoadPackages.fsx"

// Dimension
let d = 2

// Function of multiple arguments that we want to approximate
let f x = x |> List.fold (*) 1.0 

// Trapezoidal rule
// Number of points in 1D trapezoidal quadrature rule with level l.
// See Section 3.1 (p. 3)
let n1 l =
  match l with
  | 1 -> 1 // By definition
  | l when l > 20 -> failwith (sprintf "Level is too high (%A)." l)
  | l when l >= 2 -> pown 2 (l-1) + 1
  | _ -> failwith "Level l must be positive number."

// Evaluation points (abscissas) for 1D trapezodial quadrature rule at [-1; 1] for level l.
let x1 l =
  match l with
  | 1 -> [0.0]
  | l when l >= 2 -> [1 .. n1 l] |> List.map (fun i -> (double i - 1.0) * 2.0**(2.0 - double l) - 1.0)

// Weights for 1D trapezoidal quadrature rule at [-1; 1] for level l.
let w1 l =
  let mapping i = // The first and the last weight are halved.
    let weight = 2.0**(2.0 - double l)
    if i = 1 || i = n1 l then weight / 2.0 else weight
  match l with
  | 1 -> [2.0]
  | l when l >= 2 -> [1 .. n1 l] |> List.map mapping

// General quadrature rule
let quadrature w1 x1 l f =
  let ws = w1 l
  let xs = x1 l
  let mapping w x = w * f x
  List.map2 mapping ws xs |> List.sum

// Trapezoidal quadrature rule
let quadratureTrapezoidal l f =
  quadrature w1 x1 l f



// One dimensional difference grid of level l.
// See Section 4.2 (p. 7)
let theta1 l =
  match l with
  | 1 -> x1 1
  | l when l >= 2 -> (Set.ofList (x1 l) - Set.ofList (x1 (l-1))) |> Set.toList 

// Multidimensional tensor product:
// [[1; 2]; [3; 4]; [5; 6]] -->   [[1; 3; 5]; [1; 3; 6]; [1; 4; 5]; [1; 4; 6]; [2; 3; 5]; ...]
let tensorProduct (lol : 'T list list) =
  let folder item state = 
    item |> List.map (fun x -> state |> List.map (fun y -> x :: y)) |> List.collect id
  List.foldBack folder lol [[]]
// Test: tensorProduct [[1]; [3; 4]; [5; 6; 7]]


// Multi-index
// Dimension d, level (range) n
// Multi-index is vector of with d components (positive integers) ranging from 1 to n.
let allMultiIndexCombinations d n =
  List.init d (fun _ -> [1..n]) |> tensorProduct

// Returns all multi-index combinations with norm1(k) <= maxK
// Norm norm1() of multi-index is sum of its components.
let multiIndexComb maxK list =
  list |> List.filter (fun mi -> List.sum mi <= maxK)

// Function that generates all multi-indexes for all pairwise disjoint grids used in Section 4.2 (p. 7).
let multiIndexes d n =
  allMultiIndexCombinations d n |> multiIndexComb (n + d - 1)

// Creates sparse grid of points in d-dimensional space (float list list).
let x d l =
  // For a given multi-index mi create 1D difference grids and get their tensor product
  let mapping mi = mi |> List.map (fun i -> theta1 i) |> tensorProduct
  // Union of the mutually disjoint grids
  multiIndexes d l |> List.map mapping |> List.collect id

// The same as above but indexes the points
// I.e. it computes the x_kj from Section 4.3, p.8.
let x_indexed d l =
  let mapping1 k =
    theta1 k |> List.mapi (fun j theta -> ((k, j), theta))
  // For a given multi-index mi create 1D difference grids and get their tensor product
  let mapping2 mi = mi |> List.map mapping1 |> tensorProduct
  // Union of the mutually disjoint grids
  multiIndexes d l |> List.map mapping2 |> List.collect id

// Number of elements in one dimensional "difference grid" (denoted as theta) with level l.
let m l =
  match l with
  | 1 -> 1 // By definition at 
  | l when l >= 2 -> n1 l - n1 (l - 1)

// Number of points in d-dimensional sparse grid with level l. (Gerstner 1998, p.8)
let n d l = multiIndexes d l |> List.map (fun mi -> mi |> List.map m |> List.fold (*) 1) |> List.sum


// TESTS:
let points = x 2 10 |> List.map (fun [a; b] -> (a, b))
let numPoints = List.length points
FSharp.Charting.Chart.FastPoint points



// --------------------------------------------------------
// Sparse grid quadrature using the Wikipedia's formulation
// https://en.wikipedia.org/wiki/Sparse_grid
// We need to formulate the tensor product of 1D quadratures

// Example 1D quadrature
let quadrature1 f1 =
  let wi = [0.5; 1.0; 0.5]
  let xi = [-1.0; 0.0; 1.0]
  List.map2 (fun w x -> w * f1 x) wi xi |> List.sum

// Tensor product of TWO 1D quadratures
// It creates ONE 2D quadrature
let tensorProduct2 (q1 : (float -> float) -> float) (q2 : (float -> float) -> float) = 
  let quadrature (f : float list -> float) = 
    let f_outer x1 =
      let f_inner x2 = f [x1; x2]
      q2 f_inner
    q1 f_outer
  quadrature
// TEST: OK
//let quadrature2 = tensorProduct2 quadrature1 quadrature1
//let fn_v3 (xi : float list) =
//  match xi with
//  | [a; b] -> 2.0 * a * a + b * b
//  | _ -> failwith "Number of arguments is different from 2."
//quadrature2 fn_v3

// Recursive tensor product of 1D quadrature and ND quadrature
// It returns (N+1)D quadrature
let tensorProduct_v3 (q1 : (float -> float) -> float) (q2 : (float list -> float) -> float) = 
  let quadrature (f : float list -> float) = 
    let f_outer x1 =
      let f_inner x2 = f (x1 :: x2)
      q2 f_inner
    q1 f_outer
  quadrature
// TEST:
let quadratureN (f : float list -> float) =
  let auxF x =
    f [x]
  quadrature1 auxF
let quadrature2 = tensorProduct_v3 quadrature1 quadratureN
let fn_v3 (xi : float list) =
  match xi with
  | [a; b] -> 2.0 * a * a + b * b
  | _ -> failwith "Number of arguments is different from 2."
quadrature2 fn_v3
