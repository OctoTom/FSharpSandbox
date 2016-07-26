#r @"c:\Users\Tomas\Sync\Timber\LepenyNosnik\packages\MathNet.Numerics.3.11.0\lib\net40\MathNet.Numerics.dll"
#r @"c:\Users\Tomas\Sync\Timber\LepenyNosnik\packages\MathNet.Numerics.FSharp.3.11.0\lib\net40\MathNet.Numerics.FSharp.dll"
#r @"c:\Users\Tomas\OneDrive\OneSync\Projects\CommonTools\CommonTools\Bin\Release\CommonTools.dll"
#load @"c:\Users\Tomas\Scripts\Paket\LoadPackages.fsx"

open System
open System.Runtime.InteropServices
open System.IO
open System.Numerics // System.Numerics.Complex
open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra
open FSharp.Charting

// Helper functions
let c x = Complex(x, 0.0)
let i = Complex.ImaginaryOne

// == Example from Strata paper, Kottke. == 
// Layers from the top to the bottom
let rhoList = [1930.0; 2240.0] // [kg/m3]
let vsList = [350.0; 1500.0] // [m/s]
let DList = [0.07; 0.01] // [-]
let hList = [50.0] // [m] Last layer is of infinte thickness (and not in the list).

// == Example 7.3 from Kramer's book. == 
// Layers from the top to the bottom
let rhoList = [125.0; 160.0] // [kg/m3]
let vsList = [1500.0; 5000.0] // [m/s]
let DList = [0.05; 0.02] // [-]
let hList = [540.0] // [m] Last layer is of infinte thickness (and not in the list).

// Notes on 
// N = kg*m/s2
// Pa = kg/m/s2
// G = rho * vs^2
// kg/m/s2 = kg / m3 * m2 / s2

// Shear modulus. Expression taken from https://en.wikipedia.org/wiki/S-wave where it is stated beta^2 = mu / rho.
let getG rho vs = rho * vs**2.0 
// Equation (2.4a)
let getGStar G D = c G * (c 1.0 - c 2.0 * c D * c D + c 2.0 * i * c D * MathNet.Numerics.Complex.sqrt(c 1.0 - c D * c D))
// Equation (2.4b)
let getGStar_approx G D = c G * (c 1.0 + c 2.0 * i * c D)
// Equation (2.3)
let getVsStar GStar rho = MathNet.Numerics.Complex.sqrt(GStar / c rho)
let getVsStar_v2 vs D = c vs * (c 1.0 + i * c D)

let GList = List.map2 getG rhoList vsList
let GStarList = List.map2 getGStar GList DList
let vsStarList = List.map2 getVsStar GStarList rhoList
let vsStarList_v2 = List.map2 getVsStar_v2 vsList DList

// Index j starts at zero. (in the paper it is denoted as index m and it starts from 1)
let TF j n omega =
    // Equation (2.2)
    let getKStar vsStar = c omega / vsStar
    let kStarList = List.map getKStar vsStarList
    //let kStarList = List.map2 (fun vs D -> c omega / c vs * (c 1.0 - i * c D)) vsList DList
    let a_v1 j = c rhoList.[j] * vsStarList.[j] / (c rhoList.[j+1] * vsStarList.[j+1]) // aStar
    let a_v2 j = kStarList.[j] * GStarList.[j] / (kStarList.[j+1] * GStarList.[j+1] )
    let a j = a_v2 j
    let exp = MathNet.Numerics.Complex.exp
    let rec AB j =
        match j with
        | 0 -> (c 1.0, c 1.0)
        | j ->
            let (A, B) = AB (j - 1)
// This is taken from Kottke but is probably incorrect.
//          let newA = c 0.5 * A * (c 1.0 + a (j-1)) * exp(i * kStarList.[j-1] * c hList.[j-1] / c 2.0)
//                   + c 0.5 * B * (c 1.0 - a (j-1)) * exp(-i * kStarList.[j-1] * c hList.[j-1] / c 2.0)
//          let newB = c 0.5 * A * (c 1.0 - a (j-1)) * exp(i * kStarList.[j-1] * c hList.[j-1] / c 2.0)
//                   + c 0.5 * B * (c 1.0 + a (j-1)) * exp(-i * kStarList.[j-1] * c hList.[j-1] / c 2.0)
            let newA = c 0.5 * A * (c 1.0 + a (j-1)) * exp(i * kStarList.[j-1] * c hList.[j-1])
                     + c 0.5 * B * (c 1.0 - a (j-1)) * exp(-i * kStarList.[j-1] * c hList.[j-1])
            let newB = c 0.5 * A * (c 1.0 - a (j-1)) * exp(i * kStarList.[j-1] * c hList.[j-1])
                     + c 0.5 * B * (c 1.0 + a (j-1)) * exp(-i * kStarList.[j-1] * c hList.[j-1])
            newA, newB
    let (Aj, Bj) = AB j
    let (An, Bn) = AB n
    //(Aj + Bj) / (An + Bn) // Surface-Within transfer function
    (An + Bn) / (c 2.0 * An) * (Aj + Bj) / (An + Bn) // Surface-Outcrop transfer function
                                            
let tf f =
    let omega = 2.0 * System.Math.PI * f
    f, (TF 0 1 omega).Magnitude

[0.0..0.01..25.0] |> List.map tf |> FSharp.Charting.Chart.FastLine
[0.1..0.01..500.0] |> List.map tf |> List.maxBy snd

// Theoretical value of first modal frequency.
let omega0 = System.Math.PI * vsList.Head / 2.0 / hList.Head


// --------------------- //
// Play around with FFT. //
// --------------------- //
module Fourier = 
    let options = MathNet.Numerics.IntegralTransforms.FourierOptions.Default

    let f t = sin t
    let ts = [0.0..0.01..10.0]
    let fs = ts |> List.map f

    let array = fs |> List.map c |> List.toArray
    MathNet.Numerics.IntegralTransforms.Fourier.Forward(array, options)
    let invArray = array.Clone() :?> (Complex [])
    MathNet.Numerics.IntegralTransforms.Fourier.Inverse(invArray, options) 
    fs |> FSharp.Charting.Chart.Line
    array |> Seq.map (fun x -> x.Magnitude) |> FSharp.Charting.Chart.Line
    array |> Seq.map (fun x -> x.Phase) |> FSharp.Charting.Chart.Line
    array |> Seq.map (fun x -> x.Real) |> FSharp.Charting.Chart.Line
    array |> Seq.map (fun x -> x.Imaginary) |> FSharp.Charting.Chart.Line
    invArray |> Seq.map (fun x -> x.Real) |> FSharp.Charting.Chart.Line
    invArray |> Seq.map (fun x -> x.Imaginary) |> FSharp.Charting.Chart.Line // Should be zero. Small values are related to rounding error. 


// Get displacement data from file obtained from http://ngawest2.berkeley.edu/
let fileName = @"c:\Users\Tomas\Sync\Fine\Earthquake\RSN305_ITALY.P_C-CNV000.DT2"
let lines = CommonTools.IO.readLines fileName
let header = lines |> Seq.skip 3 |> Seq.head
let timeStep = 0.00244 // Sec (value extracted manually)
let valueLines = lines |> Seq.skip 4
let getValues (line : string) =
    line.Split([" "] |> List.toArray, System.StringSplitOptions.RemoveEmptyEntries) |> Seq.map float
// Test. One line of the .DT2 file looks like this.
//"   .0000000E+00   .8614203E-05   .3442699E-04   .7737549E-04   .1373918E-03" |> getValues

// Series of displacements monitored in the ITALY file. 
let values = valueLines |> Seq.collect getValues
let numPoints = values |> Seq.length
let timeIntervalLength = timeStep * (float numPoints)
values |> FSharp.Charting.Chart.FastLine

// FFT
let options = MathNet.Numerics.IntegralTransforms.FourierOptions.Default
let array = values |> Seq.map c |> Seq.toArray
MathNet.Numerics.IntegralTransforms.Fourier.Forward(array, options)
let result = array |> Seq.map (fun x -> x.Magnitude) |> Seq.toList |> List.take 100
result |> FSharp.Charting.Chart.FastLine


module ComplExp =
  let c x = System.Numerics.Complex(x, 0.0)
  let f a x = MathNet.Numerics.Complex.exp(a * c x)
  let xs = [0.0..0.01..10.0]
  let a = System.Numerics.Complex(-0.5, 10.0)
  xs |> List.map (f a) |> List.map (fun x -> x.Real) |> List.zip xs |> FSharp.Charting.Chart.Line

