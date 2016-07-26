// ---------------------------------------------------------
// Sparse grid quadrature using the Wikipedia's formulation
// https://en.wikipedia.org/wiki/Sparse_grid
// We need to formulate the tensor product of 1D quadratures
// ---------------------------------------------------------

#time
#load @"c:\Users\Tomas\Scripts\NuGet\LoadPackages.fsx"

// Type abbreviations
type tFun1D = float -> float
type tFunND = float list -> float
type tQuad1D = tFun1D -> float
type tQuadND = tFunND -> float

// Example 1D quadrature
let quadrature1D f1D =
  let wi = [0.5; 1.0; 0.5]
  let xi = [-1.0; 0.0; 1.0]
  List.map2 (fun w x -> w * f1D x) wi xi |> List.sum


// Tensor product of TWO 1D quadratures
// It creates ONE 2D quadrature
let tensorProduct1D1D (q1 : tQuad1D) (q2 : tQuad1D) = 
  let quadrature (f : float list -> float) = 
    let f_outer x1 =
      let f_inner x2 = f [x1; x2]
      q2 f_inner
    q1 f_outer
  quadrature
// TEST: OK
//let test_tensorProduct1D1D = 
//  let quadrature2D = tensorProduct1D1D quadrature1D quadrature1D
//  let fn_v3 (xi : float list) =
//    match xi with
//    | [a; b] -> 2.0 * a * a + b * b
//    | _ -> failwith "Number of arguments is different from 2."
//  quadrature2D fn_v3

// Tensor product of 1D quadrature and (N)D quadrature
// It returns (N+1)D quadrature
let tensorProduct1DND (q1 : tQuad1D) (q2 : tQuadND) = 
  let quadrature (f : tFunND) = 
    let f_outer x1 =
      let f_inner x2 = f (x1 :: x2)
      q2 f_inner
    q1 f_outer
  quadrature
// TEST:
//let test_tensorProduct1DND = 
//  let quadratureND (f : float list -> float) =
//    let auxF x = f [x]
//    quadrature1D auxF
//  let quadratureNplus1D = tensorProduct1DND quadrature1D quadratureND
//  let fn_v3 (xi : float list) =
//    match xi with
//    | [a; b] -> 2.0 * a * a + b * b
//    | _ -> failwith "Number of arguments is different from 2."
//  quadratureNplus1D fn_v3

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
  // Zero quadrature. Given any function it returns 0.0.
  //let state f = 0.0
  //qList |> List.fold add state
  qList |> List.reduce add
// Test
//let d = 2
//let l = 3
//let f [x; y] = 2.0 * x * x + y * y
//Qfull d l f
//([Qfull d l; Qfull d l ] |> sumQuadratures) f

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



// ------------------------------------------------
// Definition of Qs as they are in Wikipedia's page



// Example 1D quadrature
let Q1 l =
  match l with
  | 0 -> failwith "Level l must be positive!!!"// (fun f -> 0.0)
  | l -> quadratureTrapezoidal l
// Tests:
//let f1 (x : float) = 2.0 * x * x + 1.0
//(diffQ1D (Q1 2) (Q1 0)) f1
//(Q1 2) f1
//Q1 0 f1

// Quadrature of d-dimensional function at level l.
// Full tensor product grid, nothing sparse
let rec Qfull d l =
  // Wraps 1D quadrature as ND quadrature
  //let auxQ1 l f = Q1 l (fun x -> f [x])
  // The same as above but more readable.
  let auxQ1 l =
    let resQuad (f : float list -> float) =
      let fun1D x = f [x]
      Q1 l fun1D
    resQuad
  match d with
  | 1 -> auxQ1 l
  | d when d > 1 -> tensorProduct1DND (Q1 l) (Qfull (d-1) l)
  | _ -> failwith "Dimension d is negative."
// Test of Qfull d l:
//let fn xList =
//  match xList with
//  | [x; y] -> 2.0 * x * x + y * y
//  | _ -> failwith "List does not have two elements."
//Qfull 2 10 fn


// Quadrature of d-dimensional function at level l.
// Sparse grid defined by Smolyak's recursive formula. See https://en.wikipedia.org/wiki/Sparse_grid.
let rec Qsparse d l =
  //printfn "Qsparse %A %A" d l 
  if d < 1 then failwith "Dimension d must be positive number."
  if l < 1 then failwith "Level l must be positive number."
  if d = 1 then
    let auxQ1 l f = Q1 l (fun x -> f [x])
    auxQ1 l
  else
    let mapping i =
      let leftOperand =
        match i with
        | 1 -> Q1 i
        | _ -> diffQ1D (Q1 i) (Q1 (i - 1))
      let rightOperand = Qsparse (d - 1) (l - i + 1)
      tensorProduct1DND leftOperand rightOperand
    [1 .. l] |> List.map mapping |> sumQuadratures


// Memoizer. This function takes a function of one argument and creates its memoized version.
let memoize verbose (f:'a->'b) =
  let dict = new System.Collections.Generic.Dictionary<'a, 'b>()
  let memoizedFunction x =
    if dict.ContainsKey(x)
      then
        if verbose then printfn "Using memoized value for %A" x
        dict.[x]
      else
        if verbose then printfn "The value %A not found in dictionary. Have to compute it." x
        let y = f x
        dict.Add(x, y)
        y
  memoizedFunction

// Some test function
let f2 [x; y] =
  System.Threading.Thread.Sleep(1)
  2.0 * x * x + exp y
// Memoized version of the test function
let f2m = memoize false f2

Qfull 2 4 f2
Qsparse 2 4 f2
Qsparse 2 4 f2m

4 * 8 * 8


// Memoizer with counter
let memoizeWithCounter verbose (f:'a->'b) =
  let dict = new System.Collections.Generic.Dictionary<'a, 'b>()
  let numRetrieved = ref 0
  let numComputed = ref 0
  let memoizedFunction x =
    if dict.ContainsKey(x)
      then
        if verbose then printfn "Using memoized value for %A" x
        numRetrieved := !numRetrieved + 1
        dict.[x]
      else
        if verbose then printfn "The value %A not found in dictionary. Have to compute it." x
        numComputed := !numComputed + 1
        let y = f x
        dict.Add(x, y)
        y
  let getCounts () = (!numRetrieved, !numComputed)
  memoizedFunction, getCounts

let f x = x * x
let memF, informer = memoizeWithCounter false f
memF 1
memF 2
informer ()


