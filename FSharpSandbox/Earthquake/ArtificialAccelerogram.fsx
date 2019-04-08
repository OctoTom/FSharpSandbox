#load @"c:\Users\Tomas\Scripts\Paket\References.fsx"
#load @"Eurocode8.fsx"
#load @"ResponseSpectrum.fsx"
#load @"Fourier.fsx"
#load @"Kumar.fsx"

open System
open System.Numerics // System.Numerics.Complex
open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra
open FSharp.Charting

open Fourier
open Spectrum 
open CommonTools
open MathNet.Numerics


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

/// Updates given spectrum by multipliing its amplitudes by given factors. 
/// New complex amplitudes has the original phases while magnitudes are multiplied by the factors.
/// Frequency bins are unchanged.
let updateSpectrum factors spectrum =
  let mapping f (s : Complex) = Complex.CreatePolar(f * s.Magnitude, s.Phase)
  let newSpectralAmplitudes = List.map2 mapping factors spectrum.spectralAmplitudes
  {spectrum with spectralAmplitudes = newSpectralAmplitudes}

/// Returns elastic response spectrum in form
/// of (eigen periods Ts, max accelerations xs)
let getResponseSpectrumForFourierSpectrum xi spectrum =
  let fns = spectrum.freqBins
  // Angular eigen frequencies of SDOFs.
  let omegas_n = fns |> List.map (fun f -> 2.0 * Math.PI * f)
  let history = spectrum |> Fourier.spectrumToHistory
  let ys = history.amplitudes |> List.map (fun c -> c.Real) // REAL OR MAGNITUDE???
  let dt = history.times.[1] - history.times.[0]
  let rs = ResponseSpectrum.TIrvine.getSDOFResponseSpectrum ys dt xi omegas_n
  (fns |> List.map (fun x -> 1.0 / x), rs)

// Creates new spectrum based on the previous spectrum.
// This function represents a step in the iteration process. 
let iteration designRSFunc xi fs spectrum = 
  // Plot spectrum
  //spectrum |> Spectrum.plot (fun c -> c.Magnitude) |> Chart.Show
  let myHistory = spectrum |> Fourier.spectrumToHistory
  // Plot history
  //myHistory |> History.plot (fun c -> c.Real) |> Chart.Show
  // Response spectrum
  let (Ts, accs) = getResponseSpectrumForFourierSpectrum xi spectrum
  let points = (Ts, accs) ||> List.zip
  let designPoints = Ts |> List.mapio designRSFunc
  //// Plot response spectra
  //[
  //points |> List.tail |> Chart.FastLine
  //designPoints |> List.tail |> Chart.FastLine
  //] |> Chart.Combine |> Chart.WithXAxis(Min = 0.0, Max = 4.0) |> Chart.Show
  //[
  //points |> List.tail |> Chart.FastLine
  //designPoints |> List.tail |> Chart.FastLine
  //] |> Chart.Combine  |> Chart.WithXAxis(Log = true) |> Chart.Show
  //// Chart of only the periods corresponding to frequencies bellow Nyquist frequency.
  //[
  //points |> List.filter (fun (T, a) -> T > 2.5 / fs) |> List.tail |> Chart.FastLine
  //designPoints |> List.tail |> Chart.FastLine
  //] |> Chart.Combine |> Chart.WithXAxis(Min = 0.0, Max = 4.0) |> Chart.Show
  // Update
  //let factors = 0.0 :: List.map2 (fun a da -> da / a) (points |> List.tail |> List.map snd) (designPoints |> List.tail |> List.map snd)
  let mapping (T, p) (Tp, dp) =
    if T > 2.5 / fs && T < 10.0
      then dp / p
      else 1.0
  //let factors = List.map2 (fun (T, p) (_, dp) -> if T > 2.5 / fs && T < 10.0 then dp / p else 1.0) points designPoints
  let factors = List.map2 mapping points designPoints
  let newSpectrum = updateSpectrum factors spectrum
  newSpectrum


// Returns infinite (lazy-evaluated) sequence of Fourier spectrums
// that generate history that is compatible with prescribed responce spectrum.
let generateSpectrumCompatibleAccelerogram designRSFunc xi n fs seed =
  let initialSpectrum = createSpectrum seed n fs (fun x -> 1.0)
  // MODULATE the initial history to introduce  begining, strong-motion and ending stages of the earthquake
  let initialHistory = Fourier.Fourier.spectrumToHistory initialSpectrum
  let duration = initialHistory.times |> List.max
  let modulationFunc = Kumar.getEnvelopeFunc Kumar.eta_default Kumar.epsilon_default duration
  let modulatedComplexAmplitudes =
    (initialHistory.times, initialHistory.amplitudes)
    ||> List.map2 (fun t c -> c * Fourier.c(modulationFunc t))
  // Fourier spectrum of the modulated history
  let modulatedHistory : Fourier.History.history =
    {times = initialHistory.times; amplitudes = modulatedComplexAmplitudes} 
  let modulatedSpectrum = modulatedHistory |> Fourier.Fourier.historyToSpectrum
  // End of MODULATION
  let generator (spectrum, i) =
    let newSpectrum = iteration designRSFunc xi fs spectrum
    printfn "Iteration %A" i
    Some(newSpectrum, (newSpectrum, i+1))
  Seq.unfold generator (modulatedSpectrum, 0)
  
