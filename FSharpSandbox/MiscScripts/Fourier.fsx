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


// Playground for Forward FFT and Inverse FFT transformations.
// ===========================================================

// Helper functions
let c x = Complex(x, 0.0)
let i = Complex.ImaginaryOne
let exp = MathNet.Numerics.Complex.exp
let rand = System.Random()
module List =
  let mapio mapping list = List.map (fun x -> (x, mapping x)) list

// --------------------- //
// Play around with FFT. //
// --------------------- //
module Fourier =
  /// Spectrum
  type spectrum = {freqBins : float list; spectralAmplitudes : float list}
  /// History (time series)
  type history = {times : float list; amplitudes : float list}
  /// Scaling options of the FFT.
  let options = MathNet.Numerics.IntegralTransforms.FourierOptions.Matlab
  // Scaling of the Fourier amplitudes
  // https://youtu.be/dM1y6ZfQkDU?t=991
  let matlabAmpToRealAmp n matlabAmplitude = 2.0 * matlabAmplitude / float n
  let realAmpToMatlabAmp n realAmplitude = float n * realAmplitude / 2.0
  /// Wrapper of forward FFT
  let fftf (list : Complex list) =
    let res = (list |> List.toArray).Clone() :?> (Complex [])
    MathNet.Numerics.IntegralTransforms.Fourier.Forward(res, options) 
    res |> Array.toList
  /// Wrapper of inverse FFT 
  let ffti (list : Complex list) =
    let res = (list |> List.toArray).Clone() :?> (Complex [])
    MathNet.Numerics.IntegralTransforms.Fourier.Inverse(res, options) 
    res |> Array.toList
  /// Transforms real spectrum to real history.
  /// Only the first half of spectrum is used.
  let spectrumToHistory (spectrum : spectrum) =
    let fsHalf = spectrum.freqBins |> List.rev |> List.head
    let fs = 2.0 * fsHalf
    let dt = 1.0 / fs // Time step
    let nHalf = spectrum.freqBins |> List.length 
    let n = 2 * nHalf - 1
    let times = List.init n (fun i -> float i * dt)
    let spectralAmplitudes = spectrum.spectralAmplitudes @ List.replicate (nHalf - 1) 0.0
    let amplitudes = spectralAmplitudes |> List.map (fun x -> realAmpToMatlabAmp nHalf x |> c) |> ffti |> List.map (fun x -> x.Real)
    {times = times; amplitudes = amplitudes}
  /// Transforms real history to real spectrum
  /// Only the first half of symmetric spectrum is returned
  let historyToSpectrum (history : history) =
    let dt = history.times |> List.tail |> List.head
    let fs = 1.0 / dt
    let n = history.times |> List.length
    if n % 2 <> 1 then failwith "Number of data points in time history must be odd."
    let nHalf = (n + 1) / 2
    let freqBins = List.init n (fun i -> float i / float n * fs) |> List.take nHalf
    let spectralAmplitudes = history.amplitudes |> List.map c |> fftf |> List.map (fun x -> x.Real |> matlabAmpToRealAmp nHalf) |> List.take nHalf
    {freqBins = freqBins; spectralAmplitudes = spectralAmplitudes}

/// Eurocode spectrum in time domain
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

open Fourier
  
/// Creates spectrum given number of samples n, highest frequency fs and function defining the spectrum.
let createSpectrum nHalf fsHalf (spectrumFunc : float -> float) =
  let freqBins = [0.0 .. fsHalf / float (nHalf - 1) .. fsHalf]
  let spectralAmplitudes = freqBins |> List.map spectrumFunc
  {freqBins = freqBins; spectralAmplitudes = spectralAmplitudes}
/// Plots spectrum as line chart
let drawSpectrum (spectrum : spectrum) =
  List.zip spectrum.freqBins spectrum.spectralAmplitudes |> Chart.FastLine
/// Plots history as line chart
let drawHistory (history : history) =
  List.zip history.times history.amplitudes |> Chart.FastLine


// Get Eurocode spectrum in period domain
let mySpectrumInT = Spectrum.getSpectrum (0.1, 0.5, 2.0, 0.5, 1.0, 1.0)
// Eurocode spectrum in frequency domain
let mySpectrum f =
  match f with
  | f when f < 0.1 -> 0.0
  | f when f > 25.0 -> 0.0
  | f -> mySpectrumInT (1.0 / f)


// Sampling frequency
let fs = 100.0
// Number of points
let n = 1001
// Size of frequency bin
let df = fs / float (n - 1)

// List of random phases
let phases =
  let getRandomPhase () = rand.NextDouble() * 2.0 * System.Math.PI
  List.init n (fun _ -> getRandomPhase ())

// Bin's frequencies
let fList = [0.0 .. df .. fs]
// Bins (tuples with frequency and spectral amplitude).
let spectrumAux = fList |> List.mapio mySpectrum
let spectrum = List.map2 (fun (f, magnitude) phase -> f, Complex.CreatePolar(magnitude, phase)) spectrumAux phases
// Signal obtained by forward FFT. Phases are random.
let signal = spectrum |> List.map snd |> ffti |> List.map (fun z -> z.Real)
// Output spectrum
let outputSpectrum = signal |> List.map c |> fftf |> List.map (fun z -> z.Magnitude) 
let signalModulated =
  let mapping i x =
    match i with
    | i when i < n / 2 -> float i / float n * 2.0 * x
    | i when i < n -> (2.0 - float i / float n * 2.0) * x
    | _ -> 0.0
  signal |> List.mapi mapping
let outputSpectrumModulated = signalModulated |> List.map c |> fftf |> List.map (fun z -> z.Magnitude) 

spectrum |> List.map (fun (f, z) -> f, z.Magnitude) |> Chart.FastLine
signal |> Chart.FastLine
outputSpectrum |> Chart.FastLine
signalModulated |> Chart.FastLine
outputSpectrumModulated |> Chart.FastLine

