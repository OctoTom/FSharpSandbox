// ----------------------------------------------------------------
// Computations and notes related to the depth-dependent stiffness.
// ----------------------------------------------------------------

#load @"c:\Users\Tomas\Scripts\NuGet\LoadPackages.fsx"

// -- DATA --
// Oedometric test. Data taken from protocol number 36778 and 36779.
// Vertical stress in kPa.
let stresses = 
  [0.0; 0.05; 0.131; 0.15; 0.2; 0.3; 0.5; 0.75; 1.0; 1.5; 2.0; 3.0; 5.0;
   3.0; 2.0; 1.5; 1.0; 0.75; 0.5; 0.3] |> List.map (fun x -> 1000.0 * x)
let strains_78 =
  [0.0; 0.005; 0.006; 0.006; 0.009; 0.015; 0.022; 0.028; 0.04; 0.051; 0.068; 0.092; 0.088;
   0.083; 0.079; 0.074; 0.07; 0.066; 0.06]
let strains_79 = 
  [0.0; 0.008; 0.008; 0.008; 0.01; 0.013; 0.02; 0.028; 0.035; 0.047; 0.056; 0.071; 0.092;
   0.088; 0.085; 0.083; 0.08; 0.077; 0.075; 0.071]

// Depths at upper and lower boundary of the soil level. 
let h1 = 10.0
let h2 = 40.0

let gamma = 20.0 // kPa
let nu = 0.3
// End of DATA

// Get only the loading part of the oedometric test
let getLoadingBranch stresses strains =
  let maxStress = stresses |> List.max
  let maxIndex = stresses |> List.findIndex (fun x -> x = maxStress)
  let loadingBranchStresses = stresses |> List.toSeq |> Seq.take (maxIndex + 1) |> Seq.toList
  let loadingBranchStrains = strains |> List.toSeq |> Seq.take (maxIndex + 1) |> Seq.toList
  // Zip the stresses and strains
  let data = List.zip loadingBranchStresses loadingBranchStrains
  data

let data_78 = getLoadingBranch stresses strains_78
let data_79 = getLoadingBranch stresses strains_79

// For given oedometer data returns tangent oedometric modulus at given stress level.
// Data points are (stress, strain).
let getTangentEoed (data : (float * float) list) stress =
  // The index of first data point with stress greater or equal to given value.
  let index2 = data |> List.findIndex (fun x -> fst x >= stress)
  let (sig2, eps2) = data.[index2]
  let (sig1, eps1) = data.[index2-1]
  (sig2 - sig1) / (eps2 - eps1)

// Estimate vertical stresses
let sig1 = gamma * h1
let sig2 = gamma * h2

let Eoed1 = getTangentEoed data_79 sig1 / 1000.0 // MPa
let Eoed2 = getTangentEoed data_79 sig2 / 1000.0 // MPa

// Expression of E in terms of Eoed and nu.
let getE Eoed nu = (1.0 + nu) * (1.0 - 2.0 * nu) / (1.0 - nu) * Eoed

// Young's moduli at 
let E1 = getE Eoed1 nu
let E2 = getE Eoed2 nu

// Compute kd (gradient of E(h)
let kd = (E2 - E1) / (h2 - h1) // MPa/m

// Compute E0 (E at h=0m)
let E0 = E1 - kd * h1
let E0' = E2 - kd * h2 // Alternative 


