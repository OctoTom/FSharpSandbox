#load @"c:\Users\Tomas\Scripts\Paket\References.fsx"

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
let exp = MathNet.Numerics.Complex.exp
module List =
  let mapio mapping list = List.map (fun x -> (x, mapping x)) list

// == Example from STRATA paper by Kottke. == 
// Layers from the top to the bottom
let rhoList = [1930.0; 2240.0] // [kg/m3]
let vsList = [350.0; 1500.0] // [m/s]
let DList = [0.07; 0.01] // [-]
let hList = [50.0] // [m] Last layer is of infinte thickness (and not in the list).

// == Example 7.3 from Kramer's book. == 
// Layers from the top to the bottom
//let rhoList = [125.0; 160.0] // [kg/m3]
//let vsList = [1500.0; 5000.0] // [m/s]
//let DList = [0.05; 0.02] // [-]
//let hList = [540.0] // [m] Last layer is of infinte thickness (and not in the list).

// Notes on 
// N = kg*m/s2
// Pa = kg/m/s2
// G = rho * vs^2
// kg/m/s2 = kg / m3 * m2 / s2

// Shear modulus. Expression taken from https://en.wikipedia.org/wiki/S-wave where it is stated beta^2 = mu / rho.
let getG rho vs = rho * vs**2.0 
// Complex shear modulus. Equation (2.4a)
// D is the damping ratio. It ranges between zero (no damping) to one (complete damping).
let getGStar G D = c G * (c 1.0 - c 2.0 * c D * c D + c 2.0 * i * c D * MathNet.Numerics.Complex.sqrt(c 1.0 - c D * c D))
// Complex shear modulus in approximated form. Equation (2.4b)
// This approximation is correct for D < 0.2.
let getGStar_approx G D = c G * (c 1.0 + c 2.0 * i * c D)
// Shear wave velocity. Equation (2.3)
let getVsStar GStar rho = MathNet.Numerics.Complex.sqrt(GStar / c rho)
let getVsStar_v2 vs D = c vs * (c 1.0 + i * c D)

// Lists of G, complex G and complex wave velocity
let GList = List.map2 getG rhoList vsList
let GStarList = List.map2 getGStar GList DList
let vsStarList = List.map2 getVsStar GStarList rhoList
let vsStarList_v2 = List.map2 getVsStar_v2 vsList DList

// Returns the value of the transfer function, i.e. the ratio between
// the amplitude on the top of the j-th layer and the top of the bedrock (n-th layer)
// Index j starts at zero. (in the paper it is denoted as index m and it starts from 1)
// Omega is the angular frequency. Omaga = 2 * pi / T = 2 * pi * f
let TF j omega =
    let n = GStarList.Length - 1 // Index of the bedrock layer.
    // Complex wave number. Equation (2.2)
    let getKStar vsStar = c omega / vsStar
    let kStarList = List.map getKStar vsStarList
    // Complex impedance ratio between j-th layer and j+1-th layer. Equation (2.6)
    let a j = c rhoList.[j] * vsStarList.[j] / (c rhoList.[j+1] * vsStarList.[j+1])
    // Recursive formulas for incomming and outgoing wave amplitudes. Developed by Kramer.
    let rec AB j =
        match j with
        | 0 -> (c 1.0, c 1.0)
        | j ->
            let (A, B) = AB (j - 1)
            // This is taken from Kramer's book. The version in Kottke's STRATA manual seems to be incorrect.
            let newA = c 0.5 * A * (c 1.0 + a (j-1)) * exp(i * kStarList.[j-1] * c hList.[j-1])
                     + c 0.5 * B * (c 1.0 - a (j-1)) * exp(-i * kStarList.[j-1] * c hList.[j-1])
            let newB = c 0.5 * A * (c 1.0 - a (j-1)) * exp(i * kStarList.[j-1] * c hList.[j-1])
                     + c 0.5 * B * (c 1.0 + a (j-1)) * exp(-i * kStarList.[j-1] * c hList.[j-1])
            newA, newB
    let (Aj, Bj) = AB j
    let (An, Bn) = AB n
    // SurfaceOfJthLayer to Within transfer function, Eq (2.7).
    (Aj + Bj) / (An + Bn)
    // SurfaceOfJthLayer to Outcrop transfer function, Eq (2.9).
    //(An + Bn) / (c 2.0 * An) * (Aj + Bj) / (An + Bn)

// Amplification function.
// Parameter f is the termporal frequency.                                          
let tf f =
  // Angular frequency
  let omega = 2.0 * System.Math.PI * f
  f, (TF 0 omega).Magnitude

[0.0..0.01..25.0] |> List.map tf |> FSharp.Charting.Chart.FastLine
[0.1..0.01..500.0] |> List.map tf |> List.maxBy snd

// Theoretical value of first modal frequency.
let omega0 = System.Math.PI * vsList.Head / 2.0 / hList.Head


// --------------------- //
// Play around with FFT. //
// --------------------- //
module Fourier = 
  let options = MathNet.Numerics.IntegralTransforms.FourierOptions.Matlab
  
  // Forward FFT 
  let fftf (list : Complex list) =
    let res = (list |> List.toArray).Clone() :?> (Complex [])
    MathNet.Numerics.IntegralTransforms.Fourier.Forward(res, options) 
    res |> Array.toList

  // Inverse FFT 
  let ffti (list : Complex list) =
    let res = (list |> List.toArray).Clone() :?> (Complex [])
    MathNet.Numerics.IntegralTransforms.Fourier.Inverse(res, options) 
    res |> Array.toList
  
  // Function
  let f t = cos t
  let steps = 2.0**4.0
  let T = 2.0 * System.Math.PI
  let ts = [0.0 .. 1.0 / steps .. 1.0 - 1.0 / steps] |> List.map (fun fraction -> fraction * T)
  let fs = ts |> List.map f

  // Forward DFT
  let spectrum = fs |> List.map c |> fftf
  // Inverse DFT
  let signal = spectrum |> ffti

  // Scaling coefficient for amplitudes in spectrum for "FourierOptions.Matlab".
  let scalingCoeff = 2.0 / (float fs.Length)

  // The spectrum must be multiplied by 2.0 because the rhs mirror image is missing.
  let artifitialSpectrum = [c 0.0; c (2.0 * 1.0 / scalingCoeff)] @ List.replicate (int steps - 2) (c 0.0)
  let artifitialSignal = artifitialSpectrum |> ffti

  //----------//
  // Charting //
  //----------//

  // 1. Input signal
  fs |> FSharp.Charting.Chart.Line
  
  // 2. Spectrum
  spectrum |> List.map (fun x -> x.Magnitude) |> FSharp.Charting.Chart.Line
  spectrum |> List.map (fun x -> x.Phase) |> FSharp.Charting.Chart.Line
  spectrum |> List.map (fun x -> x.Real) |> FSharp.Charting.Chart.Line
  spectrum |> List.map (fun x -> x.Imaginary) |> FSharp.Charting.Chart.Line
  // This should hold
  spectrum.[1].Real * scalingCoeff = 1.0
  
  
  // 3. Input signal generated from spectrum
  signal |> List.map (fun x -> x.Real) |> FSharp.Charting.Chart.Line
  signal |> List.map (fun x -> x.Imaginary) |> FSharp.Charting.Chart.Line // Should be zero. Small values are related to rounding error. 

  // 4. Artificial spectrum and resulting signal
  artifitialSpectrum |> Seq.map (fun x -> x.Real) |> FSharp.Charting.Chart.Line
  artifitialSignal   |> Seq.map (fun x -> x.Real) |> FSharp.Charting.Chart.Line

  let drawBoth (mapping : Complex -> float) =
    let ch1 = signal           |> List.map mapping |> FSharp.Charting.Chart.Line
    let ch2 = artifitialSignal |> List.map mapping |> FSharp.Charting.Chart.Line
    [ch1; ch2] |> Chart.Combine

  drawBoth (fun x -> x.Imaginary)


// Play around with real data //
// -------------------------- //

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
let signalOrig = values |> Seq.toList |> List.map c
let spectrum = signalOrig |> Fourier.fftf
spectrum |> List.map (fun x -> x.Magnitude) |> List.take 100 |> Chart.FastLine

// Get truncated spectrum
let numTrunc = 100
let spectrumTrunc = (spectrum |> List.take numTrunc |> List.map (fun x -> c 2.0 * x)) @ List.init (numPoints - numTrunc) (fun _ -> c 0.0)

let signal = spectrum |> Fourier.ffti 
let signalTrunc = spectrumTrunc |> Fourier.ffti

let mapping (data : Complex list) = data |> List.map (fun x -> x.Real) |> Chart.FastLine
[signalOrig; signal; signalTrunc] |> List.map mapping |> Chart.Combine

module Spectrum =
  // For given parameters returns spectrum as a function of time period T [s].
  let getSpectrum (tb, tc, td, eta, ag, s) =
    let se t =
      match t with
      | t when t < 0.0 -> failwith "Negative period time T."
      | t when t < tb -> ag * s * (1.0 + t / tb * (2.5 * eta - 1.0))
      | t when t < tc -> 2.5 * eta * ag * s
      | t when t < td -> 2.5 * eta * ag * s * tc / t
      | t when t < 4.0 -> 2.5 * eta * ag * s * tc * td / t**2.0
      | _ -> 0.0 // For periods longer than 4 seconds.
    se

let mySpectrum = Spectrum.getSpectrum (0.1, 0.5, 2.0, 0.5, 1.0, 1.0)
let mySpectrumInF f = mySpectrum (1.0 / f)

// Required parameters for accelerogram
let dt = 0.01
let tTot = 10.0
let n = tTot / dt + 1.0 |> int
[0.0 .. dt .. tTot] |> List.mapio mySpectrum |> Chart.FastLine

// Sampling frequency
let fs = 1.0 / dt
// frequency step
let df = fs / float (n - 1)

let frequencies = [0.0 .. df .. fs]
let spectrum = frequencies |> List.map mySpectrumInF |> List.map c
let take = 200
let spectrum' = (spectrum |> List.take take) @ List.replicate (n - take) (c 0.0)
List.map2 (fun f (x : Complex) -> f, x.Real) frequencies spectrum' |> Chart.FastPoint

let times = List.init spectrum.Length (fun i -> float i * dt)
let signal = spectrum' |> Fourier.ffti
List.map2 (fun t (x : Complex) -> t, x.Real) times signal |> List.take 100 |> Chart.FastLine


List.replicate 10 (c 1.0) |> Fourier.ffti |> List.map (fun x -> x.Real) |> Chart.Line




// Draw exponent function with complex parameter
module ComplExp =
  let c x = System.Numerics.Complex(x, 0.0)
  let f a x = MathNet.Numerics.Complex.exp(a * c x)
  let xs = [0.0..0.01..10.0]
  let a = System.Numerics.Complex(-0.5, 10.0)
  xs |> List.map (f a) |> List.map (fun x -> x.Real) |> List.zip xs |> FSharp.Charting.Chart.Line

