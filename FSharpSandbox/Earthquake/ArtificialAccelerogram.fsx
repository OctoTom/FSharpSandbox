#load @"c:\Users\Tomas\Scripts\Paket\References.fsx"
#load @"Eurocode8.fsx"
#load @"ResponseSpectrum.fsx"
#load @"Fourier.fsx"

open System
open System.Numerics // System.Numerics.Complex
open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra
open FSharp.Charting

open Fourier
open Spectrum 
open CommonTools


/// Returns sequence of random phases. The sequence is cashed, i.e. it has the same items when enumerated multiple times.
/// For two different seed values two different sequences are generated.
let randomPhases seed =
  let rnd = System.Random(seed)
  Seq.initInfinite (fun _ -> 2.0 * System.Math.PI * rnd.NextDouble()) |> Seq.cache


/// Creates spectrum with n number of samples, highest temporal frequency fs
/// and function defining the *magnitudes* of spectral amplitudes.
/// Phases are random with given seed.
/// seed ... seed value used to generate random phases
/// n ... number or frequency bins
/// fs ... highest frequency of the spectrum
/// func ... mapping from frequency to magnitude (frequency ranges from 0.0 to fs)
let createSpectrum seed n fs (func : float -> float) =
  let freqBins = List.init n (fun i -> float i / float (n-1) * fs)
  let magnitudes = freqBins |> List.map func
  let phases = randomPhases seed |> Seq.take n |> Seq.toList
  let mapping mag phi = Complex.CreatePolar(mag, phi)
  let spectralAmplitudes = List.map2 mapping magnitudes phases 
  {freqBins = freqBins; spectralAmplitudes = spectralAmplitudes}

/// Takes a spectrum and list of factors
/// and returns a new spectrum with updated amplitudes.
/// New complex amplitudes has the original phases while magnitudes are multiplied by factors.
/// Frequency bins are unchanged.
let updateSpectrum factors spectrum =
  let mapping f (s : Complex) = Complex.CreatePolar(f * s.Magnitude, s.Phase)
  let newSpectralAmplitudes = List.map2 mapping factors spectrum.spectralAmplitudes
  {spectrum with spectralAmplitudes = newSpectralAmplitudes}

/// Returns elastic response spectrum in form
/// of (natural periods Ts, max accelerations xs)
let getResponseSpectrumForFourierSpectrum xi spectrum =
  let fns = spectrum.freqBins
  // Natural angular frequencies of SDOFs.
  let omegas_n = fns |> List.map (fun f -> 2.0 * Math.PI * f)
  let history = spectrum |> Fourier.spectrumToHistory
  let ys = history.amplitudes |> List.map (fun c -> c.Real) // REAL OR MAGNITUDE???
  let dt = history.times.[1] - history.times.[0]
  let rs = ResponseSpectrum.TIrvine.getSDOFResponseSpectrum ys dt xi omegas_n
  (fns |> List.map (fun x -> 1.0 / x), rs)

let n = 1000
let duration = 20.0
let dt = duration / float(n - 1)
let fs = 1.0 / dt
let xi = 0.05
// Nyquist frequency is half the fs
// Corresponding period is twice the time step

let designRSFunc = Eurocode8.EurocodeSpectrum.getHorizontalResponseSpectrum (0.15, 0.4, 2.0, 1.0, 1.0, 1.0)

//let mySpectrum = createSpectrum 0 n fs (fun x -> if x = 0.0 then 0.0 else MathNet.Numerics.Distributions.Normal.PDF(1.0, 2.0, x))
let initialSpectrum = createSpectrum 0 n fs (fun x -> 1.0)

let iteration spectrum = 
  // Plot spectrum
  spectrum |> Spectrum.plot (fun c -> c.Magnitude) |> Chart.Show
  let myHistory = spectrum |> Fourier.spectrumToHistory
  // Plot history
  myHistory |> History.plot (fun c -> c.Real) |> Chart.Show
  // Response spectrum
  let (Ts, accs) = getResponseSpectrumForFourierSpectrum xi spectrum
  let points = (Ts, accs) ||> List.zip
  let designPoints = Ts |> List.mapio designRSFunc
  // Plot response spectra
  [
  points |> List.tail |> Chart.FastLine
  designPoints |> List.tail |> Chart.FastLine
  ] |> Chart.Combine |> Chart.WithXAxis(Min = 0.0, Max = 4.0) |> Chart.Show
  [
  points |> List.tail |> Chart.FastLine
  designPoints |> List.tail |> Chart.FastLine
  ] |> Chart.Combine  |> Chart.WithXAxis(Log = true) |> Chart.Show
  // Chart of only the periods corresponding to frequencies bellow Nyquist frequency.
  [
  points |> List.filter (fun (T, a) -> T > 2.5 / fs) |> List.tail |> Chart.FastLine
  designPoints |> List.tail |> Chart.FastLine
  ] |> Chart.Combine |> Chart.WithXAxis(Min = 0.0, Max = 4.0) |> Chart.Show
  // Update
  //let factors = 0.0 :: List.map2 (fun a da -> da / a) (points |> List.tail |> List.map snd) (designPoints |> List.tail |> List.map snd)
  let factors = List.map2 (fun (T, p) (_, dp) -> if T > 2.5 / fs && T < 10.0 then dp / p else 1.0) points designPoints
  let newSpectrum = updateSpectrum factors spectrum
  newSpectrum

let spectrum1 = initialSpectrum |> iteration
let spectrum2 = spectrum1 |> iteration
let spectrum3 = spectrum2 |> iteration
let spectrum4 = spectrum3 |> iteration
let spectrum7 = spectrum4 |> iteration |> iteration |> iteration



