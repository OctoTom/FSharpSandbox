// F# syntax intro
// ---------------------------------------

// F# code compiles to .NET CLR or it can be sent to F# interactive console (REPL).

// Everything is an expression

// Literals are expressions, i.e. they have type and particular value of the type.
1 // val it : int = 1
1.0
'a'
"a"
true
[1;2] // 
(1, 1.0)
() // This is the only value the object of the unit can have.
null // null is introduced only for compatibility with .NET. It is not idiomatic since it is not a value. 

// Anonymous functions, aka function literals, lambda expressions.
fun x -> x // Identity function.
fun x -> [x] // Wraps x into single-element list.
fun x -> (x, x) // 
fun () -> x // Value x is not defined.
fun () -> 42
// With explicitly defined type
fun (x: int) -> x

// Until now, we did not apply (call) any function or operator.
// Fucntion can be applied. Application operator is just a juxtaposition: f x
f x
(fun x -> x) x
(fun x -> x) 42
(fun x -> [x]) 42
(fun x -> (x, x)) 42

// There are only functions of one argument in F#, but it can be tuple.
fun (x, y) -> [x; y]
// Or function that returns function
fun x -> (fun y -> [x; y])
// or equivalently without parenthesis
fun x -> fun y -> [x; y]
// or eqvivalently with suggered syntax
fun x y -> [x; y]

// An these can be applied
((fun x y -> [x; y]) 1) 2
(fun x y -> [x; y]) 1 2     // Function application is left associative
(fun x y -> [x; y]) 1       // is a partially applied function. It waits for input value x and makes a list.

// Predefined operators
(+) // Binary operators enclosed parenthesis are just functions of two argumets. 
(+) 1 2
((+) 1) 2
// Without parenthesis the operators are used as infix binary operators.
1 + 2

// Until now, we have not defined any named value.
// We only wrote and evaluated expressions.
// In F# we do not define variables. (thinking of variable as box containing value)
// We bind values to identifiers by the "let" keyword.
// The identifier is just a nickname for the value on the RHS of the binding expression.
// The bund values are immutable.
// Bind value to a name
let a = 42
let a = 43 // Duplicate definition of 'a'.
let b =
  let third = 14 // Any number of let bindings
  let twoThirds = 28
  third + twoThirds // The last expression is resulting value of the entire 'let' binding.
// Bind value (anonynous function) to name. Function f is no longer anonymous
let incr = fun x -> x + 1
// or equivalentyly define named function. 
let incr' x = x + 1
incr a
incr' a
incr (incr (incr a))
// Pipe operator
let (|>) x f = f x
a |> incr |> incr |> incr
 
// Functions of "multiple arguments"
let add = fun x y -> x + y
let add' x y = x + y
add' 2 (add' 3 42)
42 |> (add' 2) |> (add' 3)
42 |> add' 2 |> add' 3

// Patterns
let tuple = 1 + 1, 2 + 2
let a, b = tuple // The tuple gets decompsed
let h :: t = [1..10]

let x = []
match x with
| h::t -> sprintf "Head is %A, tail is %A." h t
| [] -> sprintf "The list is empty."

let rec map f list =
  match list with
  | h::t -> f h :: map f t
  | [] -> []
map (fun x -> 2 * x) [1..10]
map ((*) 2) [1..10]

// Reduce uses function 
let rec reduce f list =
  match list with
  | a::b::t -> reduce f (f a b :: t)
  | h::[] -> h
  | []  -> failwith "Empty list"
reduce (+) [1..10]
let sum = reduce (+)
sum [1..10]
let product = reduce (*)
let max a b = if a > b then a else b
let maxItem (list: int list) = reduce max list
maxItem [1..10]

  




// Definition of immutable value
let a = 1
// val a : int = 1
let a = 2

// Definition of a function of one argument
let twice a = 2 * a
// val twice : a:int -> int

// Equivalent as above
let twice' = fun a -> 2 * a

// Definition of a function of two arguments
let add a b = a + b
// val add : a:int -> b:int -> int

// Function value, equivalent as above.
let add' = fun a b -> a + b


// Operator (->) in type description is right associative: "int -> int -> int" is the same as  "int -> (int -> int)".
// Function Application is left associative: f x y <==> (f x) y

// This is aslo the same as the above
let add'' a =
  let incrByA b = a + b
  incrByA

List.map twice [1..10]

// Values can be piped through the functions
[1..10] |> List.map twice |> List.map (add 1) // (add 1) is partially applied function

// -- End of F# syntax intro -------------------------------------


// Construction of quadrature using sparse grid
// --------------------------------------------

// Quadrature approximates integral int f(x) =aprox= sum_i=1..n wi * f(xi)
// When f is a function of one argument I call this quadrature 1D quadrature. 

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
let diffQ1D (q1 : tQuad1D) (q2 : tQuad1D) : tQuad1D =
  let quadrature (f : tFun1D) =
    q1 f - q2 f
  quadrature

// We can even leave out the type definitions. The most general type is infered
let diffQ1D' q1 q2 =
  let quadrature f =
    q1 f - q2 f
  quadrature

// Sum list of quadratures and return 
let sumQuadratures (qList : tQuadND list) =
  let add (q1 : tQuadND) (q2 : tQuadND) : tQuadND = 
    let quadrature (f : tFunND) = q1 f + q2 f
    quadrature
  // Function reduce takes list [a; b; ... z] and operation (bullet)
  // and computes: a (bullet) b (bullet) ... (bullet) z.
  qList |> List.reduce add

// The same without the explicit type annotations
let sumQuadratures' qList =
  let add q1 q2 = 
    let resQuad f = q1 f + q2 f
    resQuad
  qList |> List.reduce add


// Tensor product of 1D quadrature and (N)D quadrature
// It returns (N+1)D quadrature
let tensorProduct1DND (q1 : tQuad1D) (q2 : tQuadND) : tQuadND = 
  let quadrature (f : tFunND) = 
    let f_outer x1 =
      let f_inner x2 = f (x1 :: x2)
      q2 f_inner
    q1 f_outer
  quadrature

// Without type annotations
let tensorProduct1DND' q1 q2 = 
  let quadrature f = 
    let f_outer x1 =
      let f_inner x2 = f (x1 :: x2)
      q2 f_inner
    q1 f_outer
  quadrature


// We need to be able to represent a 1D quadrature as ND quadrature.
let convertQ1DtoQND q1D =
  let qND fND = 
    let f1D x = fND [x]
    q1D f1D
  qND

// Quadrature of d-dimensional function at level l.
// Sparse grid defined by Smolyak's recursive formula. See https://en.wikipedia.org/wiki/Sparse_grid.
let rec Qsparse (Q1 : int -> tQuad1D) d l =
  if d < 1 then failwith "Dimension d must be positive number."
  if d = 1 then
    Q1 l |> convertQ1DtoQND
  else
    let product i =
      let lhsOperand = if i = 1 then Q1 i else diffQ1D (Q1 i) (Q1 (i - 1))
      let rhsOperand = Qsparse Q1 (d - 1) (l - i + 1)
      tensorProduct1DND lhsOperand rhsOperand
    [1 .. l] |> List.map product |> sumQuadratures
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

// How deep is my stack
let rec howDeep i =
  do printf "This is %d-th call." i
  howDeep (i + 1)

howDeep 1

let rec f i =
    if f i = 1 then // population count is one on exact powers of 2
        printf "Got up to %d\n" i
        stdout.Flush ()
    if i = 1000000000 then 0 else 1 + f (i+1)
f 0