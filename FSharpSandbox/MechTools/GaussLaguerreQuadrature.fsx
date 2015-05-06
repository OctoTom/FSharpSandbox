// Recursive definition of Laguerre polynomials
// See http://en.wikipedia.org/wiki/Laguerre_polynomial#Recursive_definition.2C_closed_form.2C_and_generating_function
let rec Laguerre n x =
  match n with
  | 0 -> 1.0
  | 1 -> -x + 1.0
  | n ->
    let k = n - 1
    1.0 / (float k + 1.0) * ((2.0  * float k + 1.0 - x) * Laguerre k x - float k * Laguerre (k-1) x)

// Roots of L2(x)
let L2Roots = [2.0 + sqrt 2.0; 2.0 - sqrt 2.0]

// Gauss-Laguerre quadrature weights
// http://en.wikipedia.org/wiki/Gauss%E2%80%93Laguerre_quadrature
let weight n x = x / (float n + 1.0)**2.0 / (Laguerre (n + 1) x)**2.0 

let wi = L2Roots |> List.map (weight 2)


// Test presnosti integral_0^inf exp(-x) * exp(-x) dx
let funcF (x:float) = exp (-x)

// Given weihts and roots
// http://mathworld.wolfram.com/Laguerre-GaussQuadrature.html
let RootsWeights n =
  match n with
  | 1 -> [(1.0, 1.0)]
  | 2 -> [(0.585786, 0.853553); (3.41421, 0.146447)]
  | 3 -> [(0.415775, 0.711093); (2.29428, 0.278518); (6.28995, 0.0103893)]
  | 4 -> [(0.322548, 0.603154); (1.74576, 0.357419); (4.53662, 0.0388879); (9.39507, 0.000539295)]
  | 5 -> [(0.26356, 0.521756); (1.4134, 0.398667); (3.59643, 0.0759424); (7.08581, 0.00361176); (12.6408, 0.00002337)]
  | _ -> failwith "Unsuported number of integration points in Gauss-Laguerre quadrature"

// Results of Gauss-Laguerre quadrature
let res n = RootsWeights n |> List.map (fun x -> snd x * funcF (fst x)) |> List.sum
let res1 = res 1 
let res2 = res 2
let res3 = res 3
let res4 = res 4

// Relative error [%]
let err res = (res - 0.5) / 0.5 *100.0 
let err1 = err res1
let err2 = err res2
let err3 = err res3
let err4 = err res4
