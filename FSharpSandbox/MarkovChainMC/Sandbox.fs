module Sandbox

open MatrixModule
open MetropolisAlgorithm

// Multiply matrices with no loops
//let multiply xs ys = xs |> List.map (fun row -> transpose ys |> List.map (fun col -> inner row col))

let a = Matrix(array2D[[1.0; 2.0]; [3.0; 4.0]])
let b = Matrix(array2D[[10.0; 20.0]; [30.0; 40.0]])
let rectangle = Matrix(array2D[[10.0; 20.0; 30.0]; [30.0; 40.0; 50.0]])
let c = rectangle.Transpose

let cholA = a.CholeskyDecomp2x2
let cholC = c.CholeskyDecomp2x2

let covMat = Matrix(array2D[[1.0; 0.6]; [0.6; 1.0]])
let res1 = covMat.CholeskyDecomp2x2


// Uloha ve Geo Sktiptech
// Element stiffness matrix #1
let B1 = Matrix(array2D[[-2.0; 0.0; 0.0; 0.0; 2.0; 0.0]; [0.0; 4.0; 0.0; -4.0; 0.0; 0.0]; [4.0; -2.0; -4.0; 0.0; 0.0; 2.0]; [0.0; 0.0; 0.0; 0.0; 0.0; 0.0]])
let B1T = B1.Transpose
let D = Matrix(array2D[[3.0; 1.0; 0.0; 1.0]; [1.0; 3.0; 0.0; 1.0]; [0.0; 0.0; 1.0; 0.0]; [1.0; 1.0; 0.0; 3.0]])
let K1 = B1T * D * B1

let B2 = MatrixModule.Matrix(array2D[[-2.0; 0.0; 2.0; 0.0; 0.0; 0.0]; [0.0; 0.0; 0.0; -4.0; 0.0; 4.0]; [0.0; -2.0; -4.0; 2.0; 4.0; 0.0]; [0.0; 0.0; 0.0; 0.0; 0.0; 0.0]])
let B2T = B2.Transpose
let K2 = B2T * D * B2

// Matice tuhosti konstrukce 

let K = Matrix(array2D[[52.0; 0.0; 8.0; -4.0]; [0.0; 28.0; -16.0; -8.0]; [8.0; -16.0; 28.0; 0.0]; [-4.0; -8.0; 0.0; 52.0]])
