module Skripta
// Vykresleni napjatosti
// Bod na dne jamy A, bod za pazenim B

// Slope of yield function in sig_m/J space.
let Mjp phi = 2.0 * sqrt(3.0) * sin phi / (3.0 + sin phi)
let linFunc a b = fun x -> a * x + b
let deg2rad deg = System.Math.PI / 180.0 * deg
// Yield function (line) drawn in sig_m/J space.
let J_yield_DP phi c =
  linFunc (-Mjp phi) (Mjp phi / tan phi * c)
// Material parameters
let phi, c = deg2rad 19.0, 10.0
// Chosen values of sig_m for which 
let sig_m = [0.0; -75.0]
// Yield function for fixed phi and c
let funcJ = J_yield_DP phi c
// List of stress points in sig_m/J space
let points = sig_m |> List.map (fun x -> x, funcJ x)
