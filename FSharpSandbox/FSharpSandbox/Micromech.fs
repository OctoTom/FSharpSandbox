module Micromech

#if INTERACTIVE
#load "Tools.fsx"
#load "Tools.fs"
#endif
 
open MathNet.Numerics.LinearAlgebra.Double
open MathNet.Numerics.LinearAlgebra.Generic
open Tools

let Pi = System.Math.PI
let halfPi = Pi/2.0

let I = CreateMatrix([[1.0; 0.0; 0.0]; [0.0; 1.0; 0.0]; [0.0; 0.0; 1.0]])
let aa = CreateMatrix([[1.0; 2.0; 4.0]]).Transpose()
let bb = I * aa

let transD phi = CreateMatrix([[cos phi; sin phi; 0.0]; [-sin phi; cos phi; 0.0]; [0.0; 0.0; 1.0]])
let transC theta = CreateMatrix([[1.0; 0.0; 0.0]; [0.0; cos theta; sin theta]; [0.0; -sin theta; cos theta]])
let transB = transD

let phi, theta, psi = Pi/180.0*28.0, 0.0, 0.0

let D = transD (phi)
let C = transC (theta)
let B = transB (psi)

let A = B * C * D

// Transformation of vector
let fixToRot_vec (a : DenseMatrix) = A * a
let rotToFix_vec (a : DenseMatrix) = A.Transpose() * a
// Tests
let a = DenseMatrix.OfMatrix(CreateMatrix([[0.2; 0.0; 0.0]]).Transpose())
let a' = fixToRot_vec a 
let a_check = rotToFix_vec a'

// Transformation of tensor
let fixToRot_tens (a : DenseMatrix) = A * a * A.Transpose() 
let rotToFix_tens (a : DenseMatrix) = A.Transpose() * a * A
// Test
let b = CreateMatrix([[1.0; 0.0; 0.0]; [0.0; 0.0; 0.0]; [0.0; 0.0; 0.0]])
let b' = fixToRot_tens b 
let b_check = rotToFix_tens (DenseMatrix.OfMatrix b')

let rotStrain = CreateMatrix([[-0.00826416; -0.126117; 0.0]; [-0.126117; 0.00713042; 0.0]; [0.0; 0.0; 0.0]])

let strain = fixToRot_tens rotStrain 



