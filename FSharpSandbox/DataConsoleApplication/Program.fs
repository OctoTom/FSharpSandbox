// DataConsoleApplication is a experimental application using FreeBase.org database.
// The project uses F#'s data providers.
module Program
open FSharp.Data
open FSharp.Net
open System.Xml.Linq

#if INTERACTIVE
#r @"C:\Users\Tomas\SkyDrive\Tech\Projects\FSharpSandbox\packages\FSharp.Data.1.1.10\lib\net40\FSharp.Data.dll"
#endif

// Infer type "Stocks" from the file "table.csv".
type Stocks = CsvProvider<"Data/table.csv", InferRows=10>
// Demonstrates a query to Yahoo.com.
// Returns first and last day and it's close price.
let firstAndLastMSFT () =
  let msft = Stocks.Load("http://ichart.finance.yahoo.com/table.csv?s=MSFT")
  let firstRecord = msft.Data |> Seq.minBy (fun x -> x.Date)
  let lastRecord = msft.Data |> Seq.maxBy (fun x -> x.Date)
  [(firstRecord.Date, firstRecord.Close); (lastRecord.Date, lastRecord.Close)]; // Return list of tuples.
let test_CSV () =
  let res1 = firstAndLastMSFT ()
  printfn "Testing data providers for finance.yahoo.com."
  printfn "First day is: %A" (fst res1.[0])
  printfn "Last day is: %A" (fst res1.[1])
  printfn "Prize changed from %A to %A in that period." (snd res1.[0]) (snd res1.[1])
  printfn ""


// Free forecast data from Yr.no.
// More info at http://om.yr.no/verdata/xml/
type ForecastType = XmlProvider<"Data/forecast.xml">
let forecastXML =
  //let content = System.IO.File.ReadAllText(@"c:\Temp\forecast.xml")
  let client = new System.Net.WebClient() // Use "new" when the type implements IDisposable
  let stream = client.OpenRead("http://www.yr.no/place/Czech_Republic/Prague/Prague/forecast.xml")
  let reader = new System.IO.StreamReader(stream)
  let content = reader.ReadToEnd()
  let feed =
    try Some (ForecastType.Parse(content))
    with | exn -> (printfn "Thrown: %A" exn.Message; None)
  feed
let test_XML () =
  let forecast = forecastXML.Value
  printfn "Location.Name: %A." forecast.Location.Name
  printfn "Location.Country: %A." forecast.Location.Country
  printfn "Sun.Rise: %A." forecast.Sun.Rise
  printfn "Sun.Set: %A." forecast.Sun.Set
  let times = forecast.Forecast.Tabular.GetTimes()
  printfn "Forecast.Tabular.GetTimes.Length: %A." (Array.length times)
  let first = times.[0]
  printfn "First.Time: From, To: %A, %A ." first.From first.To
  printfn "First.Temperature: Value, Unit: %A, %A." first.Temperature.Value first.Temperature.Unit
  printfn "First.Precipitation: %A." first.Precipitation.Value
  //printfn ": %A." forecast
  printfn "Credit: %A." forecast.Credit.Link.Text
  printfn ""
  

// Demonstrates a query to FreeBase.com
// Returns list or italian regions.
let freeBase =
  FreebaseData.GetDataContext()

let italianRegions =
  freeBase.Commons.Location.``Italian regions`` |> Seq.map (fun x -> x.Name) |> Seq.toList
let europeanCapitals =
  freeBase.Commons.Location.Countries |> Seq.map (fun x -> (x.Capital, x.``ISO Alpha 2``))
let test_FreeBase () =
  printfn "Testing data providers for freebase.org"
  printfn "FreeBase - Italian regions are: %A" italianRegions
  printfn "All capitals: %A" europeanCapitals


[<EntryPoint>]
let main argv = 
  //printfn "%A" argv
  printfn "This is \"main\" function in DataConsoleApplication."
  printfn ""

  test_CSV ()
  test_XML ()
  test_FreeBase ()

  System.Console.ReadLine() |> ignore // Wait for key
  0 // return an integer exit code

