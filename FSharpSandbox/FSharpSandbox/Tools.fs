module Tools

open MathNet.Numerics.LinearAlgebra.Double
let CreateMatrix array = DenseMatrix.ofArray2(array2D array)
printfn "File Tools.fs loaded."

