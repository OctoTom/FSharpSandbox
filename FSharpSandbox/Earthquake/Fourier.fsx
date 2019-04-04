#load @"c:\Users\Tomas\Scripts\Paket\References.fsx"

open System
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
let pi = System.Math.PI

// --------------------- //
// Play around with FFT. //
// --------------------- //
module Spectrum =
  /// Spectrum type. Frequensies are ordinary, not angular.
  type spectrum = {freqBins : float list; spectralAmplitudes : Complex list}
  /// Returns the highest ordinary frequency of the spectrum (the sampling frequency).
  let getFs (s : spectrum) = s.freqBins |> List.rev |> List.head
  /// Returns number of bins in the spectrum.
  let getN (s : spectrum) = s.freqBins |> List.length
  /// Plots spectrum as line chart.
  /// Mapping specifies which component of the complex series to plot.
  let plot (mapping : Complex -> float) (s : spectrum) =
    List.zip s.freqBins (s.spectralAmplitudes |> List.map mapping) |> Chart.FastLine |> Chart.WithXAxis(Min = 0.0)


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
  /// Plots history as line chart
  /// Mapping specifies which component of the complex series to plot.
  let plot (mapping : Complex -> float) (h : history) =
    List.zip h.times (h.amplitudes |> List.map mapping) |> Chart.FastLine |> Chart.WithXAxis(Min = 0.0)

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
  /// Transforms history to spectrum. Frequencies are ordinary, not angular. 
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

//module Fourier_TEST =
//  open Fourier
//  open History
//  open Spectrum
//  let history = {times = [0.0 .. 2.0 .. 6.0]; amplitudes = [0.0; 1.0; 2.0; 3.0] |> List.map c}
//  let spectrum = history |> Fourier.historyToSpectrum
//  let newHistory = spectrum |> spectrumToHistory
//  let newSpectrum = newHistory |> historyToSpectrum

