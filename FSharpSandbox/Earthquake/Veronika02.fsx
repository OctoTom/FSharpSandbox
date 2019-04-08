#load @"c:\Users\Tomas\Scripts\Paket\References.fsx"
#load "Eurocode8.fsx" 
#load "ArtificialAccelerogram.fsx"

open MathNet.Numerics
open FSharp.Charting
open System.Windows.Forms

let dir = __SOURCE_DIRECTORY__

let typeD = Eurocode8.EurocodeSpectrum.getHorizontalResponseSpectrum (0.2, 0.8, 2.0, 1.0, 1.0, 1.35)


let getArtificialSpectrum seed = 
  let sqn = ArtificialAccelerogram.generateSpectrumCompatibleAccelerogram typeD 0.05 1000 50.0 seed
  let num_iter = 6
  let spectrum = sqn |> Seq.skip num_iter |> Seq.head
  let history = spectrum |> Fourier.Fourier.spectrumToHistory
  let points = (spectrum.freqBins, spectrum.spectralAmplitudes |> List.map (fun c -> c.Real)) ||> List.zip
  points

let saveArtificialSpectrum seed =
  let points = getArtificialSpectrum seed
  let path = dir + @"\spectrum-" + seed.ToString() + ".csv"
  let pointToString point = 
    sprintf "%A,%A" (fst point) (snd point)
  let lines = "f[Hz],a[g]" :: List.map pointToString points
  CommonTools.IO.writeLines path lines

// --------------------------------------------------------

let getDetails seed =
  let points = getArtificialSpectrum seed
  let maxPoint = points |> List.maxBy (fun (f, a) -> a)
  let factor = 0.25
  let limitMagnitude = factor * snd maxPoint // Cut-off magnitude
  let lowerPoint = points |> List.find (fun (f, a) -> a > limitMagnitude)
  let upperPoint = points |> List.findBack (fun (f, a) -> a > limitMagnitude)
  (lowerPoint, maxPoint, upperPoint)  

let saveFrequencyLimitReport seeds =
  let filePath = dir + @"\freqencies_synthetic.csv"
  let header = "seed,f_lower(0.25)[Hz],f_max(1.0)[Hz],f_upper(0.25)[Hz]"
  let getLine seed =
    let (pl, pm, pu) = getDetails seed// Points
    sprintf "%s,%A,%A,%A" (seed.ToString()) (fst pl) (fst pm) (fst pu)
  let lines = header :: List.map getLine seeds
  CommonTools.IO.writeLines filePath lines
// --------------------------------------------------------

let seeds = [0..3]
// Run it for each file
seeds |> List.iter saveArtificialSpectrum

// Run it
saveFrequencyLimitReport seeds

// Save Spectrum Type D
let pairs = [0.0 .. 0.05 .. 4.0] |> List.map (fun T -> (T, typeD T))
let path = dir + @"\response-spectrum_type-D.csv"
let pointToString point = 
  sprintf "%A,%A" (fst point) (snd point)
let lines = "T[s],S/a_g" :: List.map pointToString pairs
CommonTools.IO.writeLines path lines



