#load @"c:\Users\Tomas\Scripts\NuGet\LoadPackages.fsx"

// Excercise 4.1
let pIll = 0.001
let pPositiveWhenIll = 0.99
let pPositiveWhenHealthy = 0.05

let pHealthy = 1.0 - pIll
let pNegativeWhenIll = 1.0 - pPositiveWhenIll
let pNegativeWhenHealthy = 1.0 - pPositiveWhenHealthy


// First data: Positive
let evidenceAfterFirstUpdate = (pPositiveWhenIll * pIll + pPositiveWhenHealthy * pHealthy)
// Probability positive after first update
let pIllWhenPositive = pPositiveWhenIll * pIll / evidenceAfterFirstUpdate
let res1 = pIllWhenPositive

// Excercise 4.2
let pHealtyWhenPositive = 1.0 - pIllWhenPositive

let evidence2 = pNegativeWhenIll * pIllWhenPositive + pNegativeWhenHealthy * pHealtyWhenPositive
let pIllWhenNegativeInSecond = pNegativeWhenIll * pIllWhenPositive / evidence2
let res2 = pIllWhenNegativeInSecond

// Excercise 4.5
let frequency (activated, study) =
  match (activated, study) with
  | (true, true) -> 166
  | (true, false) -> 199
  | (false, true) -> 703
  | (false, false) -> 2154

let res3 = 1.0 / (1.0 + 199.0 * (166.0 + 703.0) / (199.0 + 2154.0) / 166.0)


// MY EXAMPLE //

// Chose between two models:
// Model A is fair coin
// Model B is possibly unfair coin with unknown fixed probability of head (theta). The distribution of theta is uniform.

// http://rosettacode.org/wiki/Evaluate_binomial_coefficients#F.23
let binomial (n:int) (k:int) =
  let n, k = bigint n, bigint k
  List.fold (fun s i -> s * (n-i+1I)/i ) 1I [1I..k]

// -- Model A --
// fair coin with fixed probability of 0.5
let evidenceA (data:int list) = System.Math.Pow(0.5, data |> List.length |> float)
// Logarithm of evidence of n data of which k are heads
// Number of heads is not used
let lnEvA_n (n:int) = -float n * log 2.0
let lnEvA (data:int list) =
  let n = data |> List.length
  lnEvA_n n
// End of Model A

// -- Model B --
// Possibly unfaiir, biased coin with stactionar unknown probability theta
// All possible values of theta are identicaly likely.

// integral x^m*(1-x)^n from 0 to 1
let integral_v1 (m:int) (n:int) =
  let expr k = System.Math.Pow(-1.0, float k) * float (binomial n k) / float (m + k + 1)
  [0..n] |> List.sumBy expr

let integral_v2 (m:int) (n:int) =
  let m, n = bigint m, bigint n
  let nom = [1I..n] |> List.fold (*) 1I
  let den = [m+1I..m+n+1I] |> List.fold (*) 1I
  float nom / float den

let integral (m:int) (n:int) =
  let m = float m
  let product = [1..n] |> List.map float |> List.map (fun i -> i / (m + i)) |> List.fold (*) 1.0
  product / (m + float n + 1.0)

let evidenceB (data:int list) =
  let length = data |> List.length
  let n1 = data |> List.filter ((=) 1) |> List.length
  let n0 = data |> List.filter ((=) 0) |> List.length
  if n0 + n1 <> length then failwith "Wrong data in the list."
  integral n0 n1
  
// Logarithm of evidence with n samples of which k are heads (1)
let lnEvB_kn k n  =
  let expr i = log (float i / float (k + i))
  let sum = [1..(n-k)] |> List.sumBy expr
  sum - log(float(n + 1))

let lnEvB (data:int list) =
  let k = data |> List.filter ((=) 1) |> List.length
  let n = data |> List.length
  lnEvB_kn k n
// End of Model B  
  

// Generates n samples distributed by Bernoulli distribution 
// Pr(X=1) = p;  Pr(X=0) = 1 - p
let flipCoin n p =
  let rand = System.Random()
  [for i in 1..n -> if rand.NextDouble() < p then 1 else 0]

let bayesFac data = evidenceA data / evidenceB data
let bayesFac2 data = System.Math.Exp (lnEvA data - lnEvB data)

let bayesFac_kn k n = System.Math.Exp (lnEvA_n n - lnEvB_kn k n)

// Generates random data
// Computes values of Bayes factor as the data unflolds.
let getPlotData num prob =
  let flips = flipCoin num prob
  let bf i = flips |> List.toSeq |> Seq.take i |> Seq.toList |> bayesFac2
  [1..num] |> List.map (fun i -> (float i, bf i))

let getPlotData2 fromK toK n =
  [fromK..toK] |> List.map (fun k -> k, bayesFac_kn k n)

let plotData = getPlotData 10000 0.5

// Draw values of Bayes facror as a function of number of heads in da sample of n data.
//let plotData = getPlotData2 30 70 100

FSharp.Charting.Chart.FastLine plotData

