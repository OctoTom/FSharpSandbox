#load @"c:\Users\Tomas\Scripts\Paket\References.fsx"
#load @"Fourier.fsx"
#load @"ArtificialAccelerogram.fsx"

open FSharp.Charting
open MathNet.Numerics

type eqDetail =
  {
    history : Fourier.History.history;
    spectrum : Fourier.Spectrum.spectrum;
    responseSpectrum : float list * float list
  }


// REAL EARTHQUAKE
let getRealEQDetails () =
  let dir = __SOURCE_DIRECTORY__
  let lines = CommonTools.IO.readLines(dir + @"\Azerbaijan - Oguz region - ACC.ASC")
  let mapping (line : string) =
    let array = line.Split([|'\t'|])
    (float array.[0], float array.[1])
  let data = lines |> Seq.map mapping |> Seq.toList
  let history : Fourier.History.history = {times = data |> List.map fst; amplitudes = data |> List.map (snd >> Fourier.c)} 
  let spectrum : Fourier.Spectrum.spectrum = history |> Fourier.Fourier.historyToSpectrum
  let xi = 0.05
  let responseSpectrum =
    let hist = history.amplitudes |> List.map (fun c -> c.Real)
    let dt = Fourier.History.getTimeStep history
    let f_nyquist = 0.5 / dt
    let f_lowest = 1.0 / 4.0 // Corresponds to 4 second period. 
    let fs = CommonTools.Math.coverLogInterval f_lowest f_nyquist 100
    let Ts = fs |> List.map (fun f -> 1.0 / f)
    let omegas = fs |> List.map (fun f -> 2.0 * System.Math.PI * f)
    Ts, ResponseSpectrum.TIrvine.getSDOFResponseSpectrum hist dt xi omegas
  {history = history; spectrum = spectrum; responseSpectrum = responseSpectrum}

let oguz = getRealEQDetails()
oguz.history.amplitudes |> List.maxBy (fun c -> System.Math.Abs(c.Real)) |> (fun c -> c.Real)
(oguz.spectrum.freqBins, oguz.spectrum.spectralAmplitudes) ||> List.zip |> List.take 500 |> List.maxBy (fun (f, c) -> c.Real)
let history_line = Fourier.History.plot (fun c -> c.Real) oguz.history
let spectrum_line = Fourier.Spectrum.plot (fun c -> c.Real) oguz.spectrum

let truncated =
  (oguz.spectrum.freqBins |> List.map (fun f -> 2.0 * System.Math.PI * f),
  oguz.spectrum.spectralAmplitudes |> List.map (fun c -> c.Real))
  ||> List.zip
  |> List.take 500
  |> FSharp.Charting.Chart.Line

let truncated_f =
  (oguz.spectrum.freqBins |> List.map id,
  oguz.spectrum.spectralAmplitudes |> List.map (fun c -> c.Magnitude))
  ||> List.zip
  |> List.take 500
  |> FSharp.Charting.Chart.Line

let rs_line = oguz.responseSpectrum ||> List.zip |> FSharp. Charting.Chart.Line
let ag = 0.25
let drs_func = Eurocode8.EurocodeSpectrum.getHorizontalResponseSpectrum (0.2, 0.8, 2.0, 1.0, ag, 1.35)
let drs = oguz.responseSpectrum |> fst |> List.map (fun T -> T, drs_func T)
let drs_line = drs |> FSharp. Charting.Chart.Line
[rs_line; drs_line] |> FSharp.Charting.Chart.Combine
  
// ARTIFICIAL EARTHQUAKE
let getSyntheticEQDetails () =
  let n = 1024
  let dt = 0.02
  let duration = float n * dt
  let fs = 1.0 / dt
  let xi = 0.05
  // Nyquist frequency is half the fs
  // Corresponding period is twice the time step

  let designRSFunc = Eurocode8.EurocodeSpectrum.getHorizontalResponseSpectrum (0.15, 0.4, 2.0, 1.0, 1.0, 1.0)
  let sqn = ArtificialAccelerogram.generateSpectrumCompatibleAccelerogram designRSFunc xi n fs 0
  let num_iter = 10

  let spectrum = sqn |> Seq.skip num_iter |> Seq.head
  let history = spectrum |> Fourier.Fourier.spectrumToHistory
  //Fourier.History.getDuration history
  //Fourier.History.getTimeStep history
  let xi = 0.05
  let responseSpectrum =
    let hist = history.amplitudes |> List.map (fun c -> c.Real)
    let dt = Fourier.History.getTimeStep history
    let f_nyquist = 0.5 / dt
    let f_lowest = 1.0 / 4.0 // Corresponds to 4 second period. 
    let fs = CommonTools.Math.coverLogInterval f_lowest f_nyquist 100
    let Ts = fs |> List.map (fun f -> 1.0 / f)
    let omegas = fs |> List.map (fun f -> 2.0 * System.Math.PI * f)
    Ts, ResponseSpectrum.TIrvine.getSDOFResponseSpectrum hist dt xi omegas
  {history = history; spectrum = spectrum; responseSpectrum = responseSpectrum}

let synt = getSyntheticEQDetails()
synt.history.amplitudes |> List.maxBy (fun c -> System.Math.Abs(c.Real)) |> (fun c -> c.Real)
let history_line_a = Fourier.History.plot (fun c -> c.Real) synt.history
let spectrum_line_a = Fourier.Spectrum.plot (fun c -> c.Real) synt.spectrum
let rs_line_a = synt.responseSpectrum ||> List.zip |> FSharp. Charting.Chart.Line
let ag_a = 1.0
let drs_func_a = Eurocode8.EurocodeSpectrum.getHorizontalResponseSpectrum (0.15, 0.4, 2.0, 1.0, ag_a, 1.0)
let drs_a = synt.responseSpectrum |> fst |> List.map (fun T -> T, drs_func_a T)
let drs_line_a = drs_a |> FSharp. Charting.Chart.Line
[rs_line_a; drs_line_a] |> FSharp.Charting.Chart.Combine
  
