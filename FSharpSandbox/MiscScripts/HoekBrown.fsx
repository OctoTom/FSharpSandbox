#load @"c:\Users\Tomas\Scripts\NuGet\LoadPackages.fsx"

// Paper by Priest
// Table 1. (model paremeters)
//let sigma_ci = 85000.0
//let mb = 1.4
//let s = 0.0022
//let a = 0.5
// Table 3. (model paremeters)
let sigma_ci = 30000.0
let mb = 0.1499
let s = 4.467e-6
let a = 0.5611

// Hoek-Brown criterion
// sig_3 is the confining stress
// sig_1 is the major  stress at failure
let getSig1 sig3 = sig3 + sigma_ci * (mb * sig3 / sigma_ci + s)**a
let sig3List = [-1000.0..10.0..1000.0]
let sig1List = sig3List |> List.map getSig1
let line = List.zip sig3List sig1List
FSharp.Charting.Chart.Line(line)

// Normal and shear stress
let getSigNTau (sig3, sig1) = ((sig1 + sig3) / 2.0, (sig1 - sig3) / 2.0)
let line2 = line |> List.map getSigNTau
FSharp.Charting.Chart.Line(line2)


// Derived by hand on piece of paper for sig1 > sig 2 = sig3
// Using sig_m = 1/3 * (sig1 - 2*sig3)
// and J = 1/sqrt(3) * (sig1 - sig3)
let sigma_m J = sigma_ci / mb * ((sqrt(3.0) * J / sigma_ci)**(1.0 / a) - s) + 1.0 / sqrt(3.0) * J
let Js = [0.0..10.0..10000.0]
let sigma_ms = Js |> List.map sigma_m
let line3 = List.zip sigma_ms Js
FSharp.Charting.Chart.Line(line3)

// Test the above expression
let getSigMean (sig3, sig1) = 1.0 / 3.0 * (sig1 + 2.0 * sig3)
let getJ (sig3, sig1) =  1.0 / sqrt(3.0) * (sig1 - sig3)
// Gets (sig3, sig1), returns (sig_mean, J).
let mapping pair = getSigMean pair, getJ pair
let line4 = line |> List.map mapping
FSharp.Charting.Chart.Combine([FSharp.Charting.Chart.Line(line3); FSharp.Charting.Chart.Line(line4)])
// YES. The lines fits each other.

// Z Soil theory manual: 3.3.5
// Function describes the shape of the surface in deviatoric section
// Parameter e is eccentricity (0.5, 1>
// Argument theta_c (https://en.wikipedia.org/wiki/Lode_Coordinates) ranges from 0 (triax. exten.) to pi/3 (triax. compr.).
// At triaxial compression rf (pi/3) = 1
// At triaxial extension rf (0) = 1 / e
let rf e theta_c =
  let theta = theta_c
  let numerator = 4.0 * (1.0 - e**2.0) * (cos theta)**2.0 + (2.0 * e - 1.0)**2.0
  let denominator =
    2.0 * (1.0 - e**2.0) * cos theta
    + (2.0 * e - 1.0) * (4.0 * (1.0 - e**2.0) * (cos theta)**2.0 + (2.0 * e - 1.0)**2.0 - (1.0 - e**2.0))**0.5
  numerator / denominator

// Test:
//let degToRad deg = System.Math.PI / 180.0 * deg
//let thetas = [0..60] |> List.map (float >> degToRad)
//let e = 2.0 / 3.0
//let rfs = thetas |> List.map (rf e)
//let line = List.zip thetas rfs
//FSharp.Charting.Chart.Combine([FSharp.Charting.Chart.Line line])


// The yield function expressed with coefficients A, B, C and D.
// These lines are proportional for different values of theta because
// both terms with J are multiplied by coefficient rf.
let sigma_m_ABCD theta_c J =
  let A = sigma_ci / mb * (sqrt 3.0 / sigma_ci)**(1.0 / a)
  let B = 1.0 / sqrt 3.0
  let C = -1.0
  let D = - sigma_ci * s / mb
  let e = 0.5
  let r = rf e theta_c
  -(A * (r * J)**(1.0 / a) + B * (r * J) + D) / C

// The yield function expressed with coefficients A, B, C and D.
// These lines are not exactly proportional for different values of theta because
// the coefficient rf multiplies only one term with J.
let sigma_m_ABCD_v2 theta_c J =
  let A = sigma_ci / mb * (sqrt 3.0 / sigma_ci)**(1.0 / a)
  let B = 1.0 / sqrt 3.0
  let C = -1.0
  let D = - sigma_ci * s / mb
  let e = 0.5
  let r = rf e theta_c
  -(A * (J)**(1.0 / a) + B * (r * J) + D) / C

let theta_c = 0.0 // Triaxial extension
//let theta_c = System.Math.PI / 6.0
//let theta_c = System.Math.PI / 3.0 // Triaxial compression
let line5 =
  let sigma_m = Js |> List.map (sigma_m_ABCD theta_c)
  List.zip sigma_m Js
let line6 =
  let sigma_m = Js |> List.map (sigma_m_ABCD_v2 theta_c)
  List.zip sigma_m Js
// Cut the range of sigma_m
let maxSigma_m = 10000.0
let mapping2 line = line |> List.filter (fun (sigma_m, J) -> sigma_m < maxSigma_m)
[line3; line5; line6] |> List.map mapping2 |> List.map FSharp.Charting.Chart.Line |> FSharp.Charting.Chart.Combine


