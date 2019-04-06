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
  // Takes too much time - too many DoFs.
  // let responseSpectrum = ArtificialAccelerogram.getResponseSpectrumForFourierSpectrum xi spectrum


  {history = history; spectrum = spectrum; responseSpectrum = responseSpectrum}

let oguz = getRealEQDetails()
Fourier.History.plot (fun c -> c.Real) oguz.history
 
  
// ARTIFICIAL EARTHQUAKE
let n = 1024
let dt = 0.02
let duration = float n * dt
let fs = 1.0 / dt
let xi = 0.05
// Nyquist frequency is half the fs
// Corresponding period is twice the time step

let designRSFunc = Eurocode8.EurocodeSpectrum.getHorizontalResponseSpectrum (0.15, 0.4, 2.0, 1.0, 1.0, 1.0)
let sqn = ArtificialAccelerogram.generateSpectrumCompatibleAccelerogram designRSFunc xi n fs

let spectrum = sqn |> Seq.skip 8 |> Seq.head
let history = spectrum |> Fourier.Fourier.spectrumToHistory
Fourier.History.getDuration history
Fourier.History.getTimeStep history
(history.times, history.amplitudes)
||> List.map2 (fun t c -> t, c.Real)
|> FSharp.Charting.Chart.Line

let rs =
  spectrum 
  |> ArtificialAccelerogram.getResponseSpectrumForFourierSpectrum xi
  ||> List.zip
  |> List.tail // Get rid of the first (T=infinity) point
  |> List.rev
  |> List.filter (fun (T, value) -> T < 4.0) // Only periods shorter than 4s (as in eurocode response spectrum)

// Design response spectrum
let line_drs = rs |> List.map (fun (T, _) -> T, designRSFunc T) |> FSharp.Charting.Chart.Line
let line_rs = rs |> FSharp.Charting.Chart.Line
[line_drs; line_rs] |> FSharp.Charting.Chart.Combine


