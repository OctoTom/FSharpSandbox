#load @"c:\Users\Tomas\Scripts\Paket\References.fsx"
#load @"Fourier.fsx"
open FSharp.Charting
open MathNet.Numerics

let dir = __SOURCE_DIRECTORY__
let lines = CommonTools.IO.readLines(dir + @"\Azerbaijan - Oguz region - ACC.ASC")
lines |> Seq.length
let mapping (line : string) =
  let array = line.Split([|'\t'|])
  (float array.[0], float array.[1])

// let data = lines |> Seq.map mapping |> Seq.toList
// let times, values = data |> List.unzip

let f = 0.47
let T = 1.0 / f
let times = [0.0 .. 0.1 .. 10.0]
let values = times |> List.map (fun t -> sin(2.0*System.Math.PI*f*t))
let data = List.zip times values
FSharp.Charting.Chart.Line(data)

let history : Fourier.History.history = {times = times; amplitudes = values |> List.map Fourier.c}
let spectrum = Fourier.Fourier.historyToSpectrum history
let shortenedSpectrum : Fourier.Spectrum.spectrum = {freqBins = spectrum.freqBins |> List.take 200; spectralAmplitudes = spectrum.spectralAmplitudes |> List.take 200}

Fourier.Spectrum.plot (fun (c : System.Numerics.Complex) -> c.Magnitude) spectrum
Fourier.Spectrum.plot (fun (c : System.Numerics.Complex) -> c.Magnitude) shortenedSpectrum

