#load @"c:\Users\Tomas\Scripts\Paket\References.fsx"

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
  /// Maps each element x to (x, f x). (Usefull for charting)
  let mapio mapping list = List.map (fun x -> (x, mapping x)) list
module Seq =
  let takeToList n sqn = sqn |> Seq.take n |> Seq.toList

// --------------------- //
// Play around with FFT. //
// --------------------- //
module Spectrum =
  /// Spectrum type.
  type spectrum = {freqBins : float list; spectralAmplitudes : Complex list}
  /// Returns the highest frequency of the spectrum (the sampling frequency).
  let getFs (s : spectrum) = s.freqBins |> List.rev |> List.head
  /// Returns number of bins in the spectrum.
  let getN (s : spectrum) = s.freqBins |> List.length

module History =
  /// History type.
  type history = {times : float list; amplitudes : Complex list}
  /// Returns the duration of the history.
  let getDuration (h : history) = h.times |> List.rev |> List.head
  /// Returns number of time points.
  let getN (h : history) = h.times |> List.length
  /// Rerurns history with only the real parts of the imput history and zero imaginary parts.
  /// I.e. it creates only real time hisotry.
  let getRealHistory (h : history) = {h with amplitudes = h.amplitudes |> List.map (fun x -> c x.Real)}

module Fourier =
  /// Scaling options of the FFT.
  let options = MathNet.Numerics.IntegralTransforms.FourierOptions.Matlab
  // Scaling of the Fourier amplitudes
  // https://youtu.be/dM1y6ZfQkDU?t=991
  let matlabAmpToRealAmp n matlabAmplitude = matlabAmplitude / float n
  let realAmpToMatlabAmp n realAmplitude = float n * realAmplitude
  /// Wrapper of forward FFT.
  let fftf (list : Complex list) =
    let res = (list |> List.toArray).Clone() :?> (Complex [])
    MathNet.Numerics.IntegralTransforms.Fourier.Forward(res, options) 
    res |> Array.toList
  /// Wrapper of inverse FFT.
  let ffti (list : Complex list) =
    let res = (list |> List.toArray).Clone() :?> (Complex [])
    MathNet.Numerics.IntegralTransforms.Fourier.Inverse(res, options) 
    res |> Array.toList
  /// Transforms history to spectrum.
  let historyToSpectrum (h : History.history) : Spectrum.spectrum =
    let dt = h.times |> List.tail |> List.head
    let fs = 1.0 / dt
    let n = h.times |> List.length
    let freqBins = List.init n (fun i -> float i / float (n - 1) * fs)
    let spectralAmplitudes = h.amplitudes |> fftf
    {freqBins = freqBins; spectralAmplitudes = spectralAmplitudes}
  /// Transforms spectrum to history.
  let spectrumToHistory (s : Spectrum.spectrum) : History.history =
    let fs = Spectrum.getFs s
    let dt = 1.0 / fs // Time step
    let n =  Spectrum.getN s
    let times = List.init n (fun i -> float i * dt)
    let amplitudes = s.spectralAmplitudes |> ffti
    {times = times; amplitudes = amplitudes}

module Fourier_TEST =
  open Fourier
  open History
  open Spectrum
  let history = {times = [0.0 .. 2.0 .. 6.0]; amplitudes = [1.0; 0.0; 2.0; 0.5] |> List.map c}
  let spectrum = history |> Fourier.historyToSpectrum
  let newHistory = spectrum |> spectrumToHistory
  let newSpectrum = newHistory |> historyToSpectrum


/// Eurocode spectrum in SDOF period domain.
/// Horizontal elastic response spectrum
/// Defined in Eurocode 8: Design of structures for earthquake resistance, section 3.2.2.2.
module EurocodeSpectrum =
  // For given parameters returns spectrum as a function of time period T [s].
  let getSpectrum (tb, tc, td, eta, ag, s) =
    let func t =
      match t with
      | t when t < 0.0 -> failwith "Negative period time T."
      | t when t < tb -> ag * s * (1.0 + t / tb * (2.5 * eta - 1.0))
      | t when t < tc -> 2.5 * eta * ag * s
      | t when t < td -> 2.5 * eta * ag * s * tc / t
      | t when t < 4.0 -> 2.5 * eta * ag * s * tc * td / t**2.0
      | _ -> 0.0 // For periods longer than 4 seconds.
    func
module EurocodeSpectrum_TEST =
  let s = EurocodeSpectrum.getSpectrum (0.1, 0.5, 2.0, 0.5, 1.0, 1.0)
  let times = [0.0..0.01..5.0]
  times |> List.mapio s |> Chart.Line

/// Returns sequence of random phases. The sequence is cashed, i.e. it has the same items when enumerated multiple times.
/// For two different seed values two different sequences are generated.
let randomPhases seed =
  let rnd = System.Random(seed)
  Seq.initInfinite (fun _ -> rnd.NextDouble () * 2.0 * System.Math.PI) |> Seq.cache
  //Seq.initInfinite (fun _ -> rnd.NextDouble () * 1.0 * System.Math.PI) |> Seq.cache



open Fourier
open History
open Spectrum 

/// Creates spectrum with n number of samples, highest frequency fs
/// and function defining the *magnitudes* of spectral amplitudes.
/// Phases are random with given seed.
let createSpectrum seed n fs (func : float -> float) =
  let freqBins = List.init n (fun i -> float i / float (n-1) * fs)
  let magnitudes = freqBins |> List.map func
  let mapping mag phi = Complex.CreatePolar(mag, phi)
  let spectralAmplitudes = List.map2 mapping magnitudes (randomPhases seed |> Seq.takeToList n)
  {freqBins = freqBins; spectralAmplitudes = spectralAmplitudes}

/// Plots spectrum as line chart. Mapping 
let plotSpectrum (mapping : Complex -> float) (s : Spectrum.spectrum) =
  //(List.zip s.freqBins (s.spectralAmplitudes |> List.map mapping) |> Chart.FastLine).WithXAxis(Min=0.0)
  List.zip s.freqBins (s.spectralAmplitudes |> List.map mapping) |> Chart.FastLine |> Chart.WithXAxis(Min = 0.0)
/// Plots history as line chart
let plotHistory (mapping : Complex -> float) (h : History.history) =
  //(List.zip h.times (h.amplitudes |> List.map mapping) |> Chart.FastLine).WithXAxis(Min=0.0)
  List.zip h.times (h.amplitudes |> List.map mapping) |> Chart.FastLine |> Chart.WithXAxis(Min = 0.0)


// Get Eurocode spectrum in period domain
let mySpectrumInT = EurocodeSpectrum.getSpectrum (0.1, 0.5, 2.0, 0.5, 1.0, 1.0)
// Eurocode spectrum in frequency domain
let mySpectrum f =
  match f with
  | f when f < 0.1 -> 0.0
  | f when f > 25.0 -> 0.0
  | f -> mySpectrumInT (1.0 / f)
// Smooth spectrum
let mySpectrum2 f = Math.Exp(-((f - 10.0)**2.0/4.0))


/// Generates envelope function, see Kumar's paper.
/// Eta constolls amplitude at the end of series.
/// Epsilon controlls the position of maximum within the duration time.
/// Tw is the duration time.
let getEnvelopeFunc eta epsilon tw =
  let b = -epsilon * Math.Log eta / (1.0 + epsilon * (Math.Log epsilon - 1.0))
  let c = b / epsilon / tw
  let a = (Math.E / epsilon / tw)**b
  let func t = a * t**b * Math.Exp (-c * t)
  func
// Kumar's values
let envelopeFunc = getEnvelopeFunc 0.01 0.3 20.0
// Plot it.
[0.0 .. 0.1 .. 20.0] |> List.mapio envelopeFunc |> Chart.FastLine |> Chart.WithXAxis(Min = 0.0)



// Sampling frequency
let fs = 100.0
// Number of points
let n = 2000

let spectrum = mySpectrum |> createSpectrum 0 n fs
spectrum |> plotSpectrum (fun x -> x.Magnitude)
spectrum |> plotSpectrum (fun x -> x.Real)
//
let history = spectrum |> Fourier.spectrumToHistory
History.getDuration history
history |> plotHistory (fun x -> x.Real)
history |> plotHistory (fun x -> x.Imaginary)
//
let realHistory = History.getRealHistory history
realHistory |> plotHistory (fun x -> x.Real)
realHistory |> plotHistory (fun x -> x.Imaginary)
//
let newSpectrum = realHistory |> Fourier.historyToSpectrum // This spectrum is symmetric since it is specrum of real series.
newSpectrum |> plotSpectrum (fun x -> x.Magnitude) // Symmetric
newSpectrum |> plotSpectrum (fun x -> x.Real)  // Symmetric
newSpectrum |> plotSpectrum (fun x -> x.Imaginary) // Anti-Symmetric
//
let modulate h =  {h with amplitudes = List.map2 (fun t (x : Complex) -> c (envelopeFunc t * x.Real)) h.times h.amplitudes}
//
let modulatedHistory = realHistory |> modulate
modulatedHistory |> plotHistory (fun x -> x.Real)
//
let modulatedSpectrum = modulatedHistory |> Fourier.historyToSpectrum
modulatedSpectrum |> plotSpectrum (fun x -> x.Magnitude)
//
let getFactors ms =
  List.map2 (fun (orig:Complex) (modu:Complex) -> orig.Magnitude / modu.Magnitude) newSpectrum.spectralAmplitudes ms.spectralAmplitudes
  //|> List.map (fun x -> if x < 0.2 then 0.2 else x)
let factors = getFactors modulatedSpectrum
//
let getRectifiedSpectrum factors spectrum =
  let constant = 0.1
//  let mapping fact (orig:Complex) = Complex(orig.Real * (1.0 - constant + constant * fact), orig.Imaginary * (1.0 - constant + constant * fact))
  let mapping fact (orig:Complex) =
    let newMag = orig.Magnitude * (1.0 - constant + constant * fact)
    let newPhase =
      if fact > 1.2 || fact < 0.75
      then orig.Phase + 0.1 * (rand.NextDouble() - 0.5)
      else orig.Phase
    Complex.CreatePolar(newMag, newPhase)
  let newAmplitudes = List.map2 mapping factors spectrum.spectralAmplitudes
  {spectrum with spectralAmplitudes = newAmplitudes} 
let rectifiedSpectrum = getRectifiedSpectrum factors newSpectrum
rectifiedSpectrum |> plotSpectrum (fun x -> x.Magnitude)
//
let rectifiedHistory = rectifiedSpectrum |> Fourier.spectrumToHistory
rectifiedHistory |> plotHistory (fun x -> x.Real)
//
let modulatedHistory_v2 = rectifiedHistory |> modulate
modulatedHistory_v2 |> plotHistory (fun x -> x.Real)
//
let modulatedSpectrum_v2 = modulatedHistory_v2 |> Fourier.historyToSpectrum
modulatedSpectrum_v2 |> plotSpectrum (fun x -> x.Magnitude)
// --
let factors2 = getFactors modulatedSpectrum_v2
//
let rectifiedSpectrum_v2 = getRectifiedSpectrum factors2 rectifiedSpectrum
rectifiedSpectrum_v2 |> plotSpectrum (fun x -> x.Magnitude)
//
let rectifiedHistory_v2 = rectifiedSpectrum_v2 |> Fourier.spectrumToHistory
rectifiedHistory_v2 |> plotHistory (fun x -> x.Real)
//
let modulatedHistory_v3 = rectifiedHistory_v2 |> modulate
modulatedHistory_v3 |> plotHistory (fun x -> x.Real)
//
let modulatedSpectrum_v3 = modulatedHistory_v3 |> Fourier.historyToSpectrum
modulatedSpectrum_v3 |> plotSpectrum (fun x -> x.Magnitude)
// --
let factors3 = getFactors modulatedSpectrum_v3
//
let rectifiedSpectrum_v3 = getRectifiedSpectrum factors3 rectifiedSpectrum_v2
rectifiedSpectrum_v3 |> plotSpectrum (fun x -> x.Magnitude)
//
let rectifiedHistory_v3 = rectifiedSpectrum_v3 |> Fourier.spectrumToHistory
rectifiedHistory_v3 |> plotHistory (fun x -> x.Real)
//
let modulatedHistory_v4 = rectifiedHistory_v3 |> modulate
modulatedHistory_v4 |> plotHistory (fun x -> x.Real)
//
let modulatedSpectrum_v4 = modulatedHistory_v4 |> Fourier.historyToSpectrum
modulatedSpectrum_v4 |> plotSpectrum (fun x -> x.Magnitude)

let iteration (rs,  ms) = 
  let factors = getFactors ms
  let rsNew = getRectifiedSpectrum factors rs
  let rhNew = rsNew |> Fourier.spectrumToHistory
  let mhNew = rhNew |> modulate
  let msNew = mhNew |> Fourier.historyToSpectrum
  rsNew, msNew

let compositeIteration = List.replicate 100 iteration |> List.reduce (>>)

let (rs6, ms6) = compositeIteration (rectifiedSpectrum_v3, modulatedSpectrum_v4)
ms6 |> plotSpectrum (fun x -> x.Magnitude)
ms6 |> Fourier.spectrumToHistory |> plotHistory (fun x -> x.Real)

let (rs7, ms7) = iteration (rs6, ms6)
ms7 |> plotSpectrum (fun x -> x.Magnitude)
ms7 |> Fourier.spectrumToHistory |> plotHistory (fun x -> x.Real)




// Is supperposition of two modulated signals modulated? //
// Yes, it is.
// ---------------------------------------------------- //
