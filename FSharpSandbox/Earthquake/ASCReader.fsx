#load @"c:\Users\Tomas\Scripts\Paket\References.fsx"
#load @"Fourier.fsx"

// Stuff to read from the ASC file
type ASCData =
  {
  date : string
  timeAccPairs : (float * float) list
  }

// Reads ASC file and returns the earthqake's date and list of time-acceleration pairs.
let readASCFile filePath = 
  let lines = CommonTools.IO.readLines(filePath) |> Seq.toList
  let date = (lines |> List.find (fun line -> line.StartsWith("EVENT_DATE_YYYYMMDD"))).Split([|':'|]).[1].Trim()
  let timeStep = (lines |> List.find (fun line -> line.StartsWith("SAMPLING_INTERVAL_S"))).Split([|':'|]).[1] |> float
  let values = lines |> List.skipWhile (fun line -> line.Contains(":")) |> List.map float
  let numRecords = values |> List.length
  let times = Seq.init numRecords (fun i -> float i * timeStep) |> Seq.toList
  let timeAccPairs =  (times, values) ||> List.zip
  {date = date; timeAccPairs = timeAccPairs}

// Reads ASC file and returns  
let readASCFileToHistory filePath : string * Fourier.History.history = 
  let result = readASCFile filePath
  let date = result.date
  let times, amplitudes = result.timeAccPairs |> List.unzip
  date, {times = times; amplitudes = amplitudes |> List.map Fourier.c}

