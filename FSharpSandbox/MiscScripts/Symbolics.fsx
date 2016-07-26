#load @"c:\Users\Tomas\Scripts\Paket\LoadPackages.fsx"
#load @"c:\Users\Tomas\OneDrive\OneSync\Projects\FSharpSandbox\FSharpSandbox\packages\MathNet.Symbolics.0.9.0\MathNet.Symbolics.fsx"

open System.Numerics
open MathNet.Numerics
open MathNet.Symbolics

open Operators

let x = symbol "x"
let y = symbol "y"
let z = symbol "z"
let a = symbol "a"
let b = symbol "b"
let c = symbol "c"
let d = symbol "d"
let e = symbol "e"
let f = symbol "f"


let ex1 = 3 * x * x + 11 * 10 - 50
MathNet.Symbolics.Algebraic.factors ex1
MathNet.Symbolics.Algebraic.separateFactors x ex1
MathNet.Symbolics.Algebraic.expand ex1
MathNet.Symbolics.Algebraic.summands ex1
MathNet.Symbolics.Algebraic.expand ex1
MathNet.Symbolics.Algebraic.expand ex1
MathNet.Symbolics.Polynomial.coefficients x ex1
MathNet.Symbolics.Polynomial.variables ex1


