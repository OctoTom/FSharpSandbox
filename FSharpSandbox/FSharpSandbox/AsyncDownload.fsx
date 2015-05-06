open System.Net
open Microsoft.FSharp.Control.WebExtensions
open System.IO

type TickerType =
  /// Stock Market Index.
  | Index = 0
  /// Exchange-traded fund.
  | ETF = 1
  /// Stock listed in Dow Jones Industrial Average index (DJI).
  | Dow30Stock = 2
  /// Fund at ProFunds.
  | ProFund = 3
  /// Volatility index CBOE.
  | VolatilityIndex = 4
  /// Advancer / Decliner
  | AdvancerDecliner = 5
  /// Tick Indices
  | UpVolume = 6
  /// Tick Indices
  | TickIndex = 7

type TickerRecord = {
  Ticker : string;
  Description : string;
  Type : TickerType;
  Source : string;
  Url : string
  }

let stringToTickerType s =
  System.Enum.Parse(typeof<TickerType>, s) :?> TickerType

let header = "Ticker,Description,Type,Source,Url"
let tickerRecords =
  let lines = System.IO.File.ReadAllLines(@"C:\Users\Tomas\Documents\Tickers.csv") |> Array.toList
  let data =
    match lines with
    | [] -> failwith "Empty file"
    | h::[] -> failwith "File contains only header and no data."
    | h::t when h = header -> t
    | _ -> failwith "First line of the .csv file does not match the expected header"
  let parseLine (line : string) =
    let strings = line.Split([|','|])
    {Ticker = strings.[0]; Description = strings.[1]; Type = stringToTickerType strings.[2]; Source = strings.[3]; Url = strings.[4]}
  data |> List.map parseLine

let templateYahoo = "http://ichart.finance.yahoo.com/table.csv?s="

let getTickerRecordUrl tr =
  match tr with
  // If there is non-empty url return it.
  | {Ticker = t; Source = s} when s.Contains("http") -> (t, s)
  // If type is Index -> create yahoo url using ^ticker
  | {Ticker = t; Source = s} when s = "YahooIndex" -> (t, templateYahoo + "^" + t)  
  // If type is ETF or Dow30 -> create yahoo url using ticker
  | {Ticker = t; Source = s} when s = "Yahoo" -> (t, templateYahoo + t)  
  | {Ticker = t} -> failwith ("No Url for ticker " + t + ".")

let urls =
  tickerRecords |>
  List.filter (fun x -> not (System.String.IsNullOrEmpty x.Source)) |>
  List.map (fun x -> getTickerRecordUrl x)

// Add ProFund file (data of all profunds are stored in single file) 
let urlsAll =
  ("_historical_nav_", "https://accounts.profunds.com/aadata/historical_nav.csv") :: urls
  

let fetchAsync(name, url:string) =
    async { 
        try 
            let uri = new System.Uri(url)
            let webClient = new WebClient()
            let! html = webClient.AsyncDownloadString(uri)
            System.IO.File.WriteAllText("C:\\Users\\Tomas\\Temp\\" + name + ".csv", html)
            printfn "Read %d characters for %s" html.Length name
        with
            | ex -> printfn "%s" (ex.Message);
    }

let downloadAsync urls =
    urls
    |> Seq.map fetchAsync
    |> Async.Parallel 
    |> Async.RunSynchronously
    |> ignore

downloadAsync urlsAll