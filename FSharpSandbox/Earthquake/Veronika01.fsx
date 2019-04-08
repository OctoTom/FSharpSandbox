#load @"c:\Users\Tomas\Scripts\Paket\References.fsx"
#load "Fourier.fsx"
#load "ASCReader.fsx"

open MathNet.Numerics
open FSharp.Charting
open System.Windows.Forms

let dir = __SOURCE_DIRECTORY__

// For given file name returns full path.
let getFilePath fileName =
  dir + @"\" + fileName

// List of ASC file names.
let fileNames =
  [
    "IU.GNI.20.HN1.D.20121007.114249.C.ACC.ASC"
    "IU.GNI.20.HN1.D.20140210.120646.C.ACC.ASC"
    "IU.GNI.20.HN1.D.20150904.044938.C.ACC.ASC"
    "IU.GNI.20.HN1.D.20170511.032420.C.ACC.ASC"
  ]

// --------------------------------------------------------

// For given fourier spectrum returns list of (f, a) pairs.
// The list is truncated to 5 Hz.
let truncatedSpectrum_magnitudes (spectrum : Fourier.Spectrum.spectrum) =
  (spectrum.freqBins, spectrum.spectralAmplitudes |> List.map (fun c -> c.Magnitude))
  ||> List.zip
  |> List.takeWhile (fun (f, _) -> f < 5.0)

// For given file name returns truncated spectrum as list of (f, a) pairs.
let getSpectrum fileName =
  fileName
  |> getFilePath
  |> ASCReader.readASCFileToHistory
  |> snd
  |> Fourier.Fourier.historyToSpectrum
  |> truncatedSpectrum_magnitudes

// Saves Fourier spectrum of given file in .csv format.
let saveSpectrumData fileName =
  let path = dir + @"\spectrum-" + fileName + ".csv"
  let spectrum = getSpectrum fileName
  let pointToString point = 
    sprintf "%A,%A" (fst point) (snd point)
  let lines = "f[Hz],a[cm/s^2]" :: List.map pointToString spectrum
  CommonTools.IO.writeLines path lines

// --------------------------------------------------------

// Returns range of dominating frequencies computed form Fourier spectrum.
let getFrequencyRange fileName =
  let spectrum = getSpectrum fileName
  let maxPoint = spectrum |> List.maxBy (fun (f, a) -> a)
  let factor = 0.25
  let limitMagnitude = factor * snd maxPoint // Cut-off magnitude
  let lowerPoint = spectrum |> List.find (fun (f, a) -> a > limitMagnitude)
  let upperPoint = spectrum |> List.findBack (fun (f, a) -> a > limitMagnitude)
  (lowerPoint, maxPoint, upperPoint)  

// Iterates over given files, extracts frequency ranges and saves them to .csv file.
let saveFrequencyLimitReport fileNames =
  let filePath = dir + @"\freqencies.csv"
  let header = "file_name,f_lower(0.25)[Hz],f_max(1.0)[Hz],f_upper(0.25)[Hz]"
  let getLine fileName =
    let (pl, pm, pu) = getFrequencyRange fileName // Points
    sprintf "%s,%A,%A,%A" fileName (fst pl) (fst pm) (fst pu)
  let lines = header :: List.map getLine fileNames
  CommonTools.IO.writeLines filePath lines

// --------------------------------------------------------

// Run it for each file
fileNames |> List.iter saveSpectrumData
// Run it
saveFrequencyLimitReport fileNames




