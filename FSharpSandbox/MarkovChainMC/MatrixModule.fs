module MatrixModule
  // Function used in matrix multiplication.
  let mySum n f =
    [0..n] |> List.fold (fun state item -> state + f item) 0.0

  type Matrix(xs: float[,]) =
    let xs = Array2D.copy xs
    member this.mRows = xs.GetLength 0 // m .. Number of rows,  "this" can be "x" or even "__" (double underscore)
    member this.nColumns = xs.GetLength 1 // n .. Number of columns
    member this.Item(i, j) = xs.[i, j]
    member this.Transpose = Matrix.init this.nColumns this.mRows (fun i j -> this.[j, i])
    /// Cholesky decompositon of 2x2 symmetric matrix with 1.0s on diagonal (i.e. covariant matrix)
    member this.CholeskyDecomp2x2 =
      if (this.mRows <> 2 || this.nColumns <> 2 || this.[0,1] <> this.[1,0]) then raise (new System.Exception("Matrix is not 2x2."))
      let A = this.[0,0]
      let B = this.[0,1]
      let C = this.[1,1]
      let a = sqrt(A)
      let b = B / a
      let c = sqrt(C - b * b)
      Matrix(array2D[[a; 0.0]; [b; c]])
    member this.Invert3x3 =
      if (this.mRows <> 3 || this.nColumns <> 3) then raise (new System.Exception("Matrix is not 3x3."))
      let a = this.[0,0]
      let b = this.[0,1]
      let c = this.[0,2]
      let d = this.[1,0]
      let e = this.[1,1]
      let f = this.[1,2]
      let g = this.[2,0]
      let h = this.[2,1]
      let i = this.[2,2]
      let det = a*(e*i-f*h)-b*(i*d-f*g)+c*(d*h-e*g)
      let A = (e*i-f*h) / det
      let D = -(b*i-c*h) / det
      let G = (b*f-c*e) / det
      let B = -(d*i-f*g) / det
      let E = (a*i-c*g) / det
      let H = -(a*f-c*d) / det
      let C = (d*h-e*g) / det
      let F = -(a*h-b*g) / det
      let I = (a*e-b*d) / det
      Matrix(array2D[[A; D; G]; [B; E; H]; [C; F; I]])
    override this.ToString() = sprintf "%A" xs
    static member init m n f = Matrix(Array2D.init m n f) // Initializes the matrix
    static member (+) (a: Matrix, b: Matrix) =
      Matrix.init a.mRows a.nColumns (fun i j -> a.[i, j] + b.[i, j])
    static member (-) (a: Matrix, b: Matrix) =
      Matrix.init a.mRows a.nColumns (fun i j -> a.[i, j] - b.[i, j])
    static member (*) (a: Matrix, b: Matrix) =
      Matrix.init a.mRows b.nColumns (fun i j -> mySum (a.nColumns - 1) (fun k -> a.[i, k] * b.[k, j]))
  
  let covMat = Matrix(array2D[[2.0; 2.0]; [2.0; 2.0]])
  let res1 = covMat.CholeskyDecomp2x2
