// Definition of immutable value
let a = 1
// val a : int = 1

// Definition of a function of one argument
let twice a = 2 * a
// val twice : a:int -> int

// Definition of a function of two arguments
let add a b = a + b
// val add : a:int -> b:int -> int

// Operator (->) is right associative: int -> int -> int <==> int -> (int -> int)
// Function Application is left associative: f x y <==> (f x) y

// The above is in fact this
let add' a =
  let incrByA b = a + b
  incrByA

List.map twice [1..10]
[1..10] |> List.map twice |> List.map (add 1)

// ---------------------------------------

// Construction of quadrature using sparse grid
// --------------------------------------------

// Quadrature approximates integral int f(x) =aprox= sum_i=1..n wi * f(xi)
// I call this quadrature 1D. The function is of one argument.

// Multidimensional quadratures can be constructed by tensor product but it leads to "curse of dimensionality".
// Smolyak suggested not to use full tensor product grid but only a subset of the points.

// Here comes my confusion:
// In Numerical Integration using Sparse Grids by T.Gerstner and M. Griebel (open the article),
// we have quite complex definition of the points and even more intimidating formulation of their weights.
// On the other hand on https://en.wikipedia.org/wiki/Sparse_grid we have just one formula and no points or weights are even mentioned.

// Thus quadrature is a functional. It takes function and returns number


// Type abbreviations
type tFun1D = float -> float
type tQuad1D = tFun1D -> float // This is the same as: (float -> float) -> float

type tFunND = float list -> float
type tQuadND = tFunND -> float

// To be able to code the Smolyak's formula on Wikipedia we need to define the following things:
// 1. Difference of 1D quadratures
// 2. Sum of several ND quadratures
// 3. Tensor product of 1D quadrature and ND quadrature

// Difference 1D quadrature
let diffQ1D (q1 : tQuad1D) (q2 : tQuad1D) =
  let quadrature (f : tFun1D) =
    q1 f - q2 f
  quadrature

// Sum list of quadratures and return 
let sumQuadratures (qList : tQuadND list) =
  let add (q1 : tQuadND) (q2 : tQuadND) : tQuadND = 
    let resQuad (f : tFunND) = q1 f + q2 f
    resQuad
  qList |> List.reduce add

// Tensor product of 1D quadrature and (N)D quadrature
// It returns (N+1)D quadrature
let tensorProduct1DND (q1 : tQuad1D) (q2 : tQuadND) = 
  let quadrature (f : tFunND) = 
    let f_outer x1 =
      let f_inner x2 = f (x1 :: x2)
      q2 f_inner
    q1 f_outer
  quadrature

// We need to 
let convertQ1DtoQND q1 =
  let qN (fN) = 
    let f1 x = fN [x]
    q1 f1
  qN

// Quadrature of d-dimensional function at level l.
// Sparse grid defined by Smolyak's recursive formula. See https://en.wikipedia.org/wiki/Sparse_grid.
let rec Qsparse (Q1 : int -> tQuad1D) d l =
  if d < 1 then failwith "Dimension d must be positive number."
  if d = 1 then
    Q1 l |> convertQ1DtoQND
  else
    let mapping i =
      let leftOperand =
        match i with
        | 1 -> Q1 i
        | _ -> diffQ1D (Q1 i) (Q1 (i - 1))
      let rightOperand = Qsparse Q1 (d - 1) (l - i + 1)
      tensorProduct1DND leftOperand rightOperand
    [1 .. l] |> List.map mapping |> sumQuadratures
// The Smolyak's formula is completed. Let's test it.

// We have to create 1D quadrature.
// General quadrature rule
let getQuadrature w1 x1 l =
  let ws = w1 l
  let xs = x1 l
  let res (f : float -> float) = 
    let mapping w x = w * f x
    List.map2 mapping ws xs |> List.sum
  res

// ---------------------------------
// Trapezoidal rule
// Number of points in 1D trapezoidal quadrature rule with level l.
// See Section 3.1 (p. 3)

// Trapezoidal quadrature rule
let quadratureTrapezoidal l =
  let n1 l =
    match l with
    | 1 -> 1 // By definition
    | l when l > 20 -> failwith (sprintf "Level is too high (%A)." l)
    | l when l >= 2 -> pown 2 (l-1) + 1
    | _ -> failwith "Level l must be positive number."
  // Evaluation points (abscissas) for 1D trapezoidal quadrature rule at [-1; 1] for level l.
  let x1 l =
    match l with
    | 1 -> [0.0]
    | l -> [1 .. n1 l] |> List.map (fun i -> (double i - 1.0) * 2.0**(2.0 - double l) - 1.0)
  // Weights for 1D trapezoidal quadrature rule at [-1; 1] for level l.
  let w1 l =
    let mapping i = // The first and the last weight are halved.
      let weight = 2.0**(2.0 - double l)
      if i = 1 || i = n1 l then weight / 2.0 else weight
    match l with
    | 1 -> [2.0]
    | l -> [1 .. n1 l] |> List.map mapping
  getQuadrature w1 x1 l

let Q1 l = quadratureTrapezoidal l

let Q2s = Qsparse Q1 2 4

// Some test function
let f2 [x; y] =
  //System.Threading.Thread.Sleep(1)
  printfn "Function f2() called with x=%A and y=%A" x y
  2.0 * x * x + exp y
// Memoized version of the test function
let f2m = memoize false f2

Q2s f2

