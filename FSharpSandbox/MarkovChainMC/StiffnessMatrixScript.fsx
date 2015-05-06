let sum i0 i1 f =
    let mutable t = 0.0
    for i in i0..i1 do
        t <- t + f i
    t
    
type Matrix(xs: float [,]) =
  let xs = Array2D.copy xs

  member __.Rows = xs.GetLength 0

  member __.Columns = xs.GetLength 1

  member this.Item(i, j) = xs.[i, j]

  override this.ToString() = sprintf "%A" xs

  static member init m n f = Matrix(Array2D.init m n f)

  static member (~-) (a: Matrix) =
    Matrix.init a.Rows a.Columns (fun i j -> -a.[i, j])

  static member (+) (a: Matrix, b: Matrix) =
    Matrix.init a.Rows a.Columns (fun i j -> a.[i, j] + b.[i, j])

  static member (-) (a: Matrix, b: Matrix) =
    Matrix.init a.Rows a.Columns (fun i j -> a.[i, j] - b.[i, j])

  static member (*) (a: Matrix, b: Matrix) =
    Matrix.init a.Rows b.Columns (fun i j ->
      sum 0 (b.Rows-1) (fun k -> a.[i, k] * b.[k, j]))

let matrix xss = Matrix(array2D xss)

let (|Scalar|Blocks|) (a: Matrix) =
  let m, n = a.Rows, a.Columns
  if m=1 && n=1 then Scalar a.[0, 0] else
  Blocks((Matrix.init (m/2) (n/2) (fun i j -> a.[i, j]),
          Matrix.init (m/2) (n - n/2) (fun i j -> a.[i, j + n/2])),
         (Matrix.init (m - m/2) (n/2) (fun i j -> a.[i + m/2, j]),
          Matrix.init (m - m/2) (n - n/2) (fun i j -> a.[i + m/2, j + n/2])))
    
let Scalar x = Matrix.init 1 1 (fun _ _ -> x)

let Blocks((a: Matrix, b: Matrix), (c: Matrix, d: Matrix)) =
  let m0, n0, m1, n1 = a.Rows, a.Columns, d.Rows, d.Columns
  Matrix.init (m0 + m1) (n0 + n1) (fun i j ->
    match i < m0, j < n0 with
    | true, true -> a.[i, j]
    | true, false -> b.[i, j - n0]
    | false, true -> c.[i - m0, j]
    | false, false -> d.[i - m0, j - n0])

let rec invert = function
  | Scalar x -> Scalar(1.0 / x)
  | Blocks((A, B), (C, D)) ->
      let iA = invert A
      let iAB = iA * B
      let E = invert (D - C * iAB)
      let ECiA = E * C * iA
      Blocks((iA + iAB * ECiA, - iAB * E), (-ECiA, E))

let K = matrix [[52.0; 0.0; 8.0; -4.0]; [0.0; 28.0; -16.0; 8.0]; [8.0; -16.0; 28.0; 0.0]; [-4.0; 8.0; 0.0; 52.0]]
let Kinv = invert K
let f = matrix [[-40.0]; [0.0]; [0.0]; [-40.0]]
let r = (Kinv * f)
let rr = [0..3] |> List.map (fun i -> r.Item(i, 0) / 50.0) // We have to devide the results by 50, which is the multiplicator of the global stiffness matrix.
let eps_y = rr.[0] / 2.0
let eps_x = rr.[1] / 4.0

let res3 = 0.25 / 0.75 * 0.009375

let B1 = matrix [[-2.0; 0.0; 0.0; 0.0; 2.0; 0.0]; [0.0; 4.0; 0.0; -4.0; 0.0; 0.0]; [4.0; -2.0; -4.0; 0.0; 0.0; 2.0]; [0.0; 0.0; 0.0; 0.0; 0.0; 0.0]]
let B2 = matrix [[-2.0; 0.0; 2.0; 0.0; 0.0; 0.0]; [0.0; 0.0; 0.0; -4.0; 0.0; 4.0]; [0.0; -2.0; -4.0; 2.0; 4.0; 0.0]; [0.0; 0.0; 0.0; 0.0; 0.0; 0.0]]
let r1 = matrix [[0.0]; [rr.[0]]; [0.0]; [0.0]; [rr.[2]]; [rr.[3]]]
let r2 = matrix [[0.0]; [0.0]; [rr.[1]]; [0.0]; [rr.[2]]; [rr.[3]]]
let eps_1 = B1 * r1
let eps_2 = B2 * r2
let J = 8.0 
let eps1 = matrix ([0..3] |> List.map (fun i -> [eps_1.Item(i, 0) / J]))
let eps2 = matrix ([0..3] |> List.map (fun i -> [eps_2.Item(i, 0) / J]))
let D = matrix ([[3.0; 1.0; 0.0; 1.0]; [1.0; 3.0; 0.0; 1.0]; [0.0; 0.0; 1.0; 0.0]; [1.0; 1.0; 0.0; 3.0]] |> List.map (fun x -> x |> List.map (fun x -> x * 800.0)))
let sig1 = D * eps1
let sig2 = D * eps2
