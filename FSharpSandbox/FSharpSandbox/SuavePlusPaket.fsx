// Self-contained Suave example
// https://suave.io/paket.html

// TPetricek's dojo
// https://github.com/tpetricek/Dojo-Suave-FsHome

// Simple Suave web app (to-do list)
// https://github.com/clausasbjorn/suave-todo

#r "System.Xml.Linq.dll" // This is needed for provided types to show intellisense. Send reference to fsi is not sufficient.
#load @"c:\Users\Tomas\Scripts\NuGet\LoadPackages.fsx"
// Step 0. Boilerplate to get the paket.exe tool
 
open System
open System.IO
 
Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
 
if not (File.Exists "paket.exe") then
    let url = "https://github.com/fsprojects/Paket/releases/download/0.31.5/paket.exe"
    use wc = new Net.WebClient()
    let tmp = Path.GetTempFileName()
    wc.DownloadFile(url, tmp)
    File.Move(tmp,Path.GetFileName url);;
 
// Step 1. Resolve and install the packages
 
#r "paket.exe"
 
Paket.Dependencies.Install """
source https://nuget.org/api/v2
nuget Suave
""";;
 
// Step 2. Use the packages
 
#r "../packages/Suave.1.0.0/lib/net40/Suave.dll"
 
open Suave // always open suave

// APPLICATION NO.1 
let okWebPart = Suave.Successful.OK "Ahoj!" 

open Suave
open Suave.Operators

// APPLICATION NO.2
let app_2 : WebPart =
  choose
    [ Suave.Filters.path "/" >=> Suave.Successful.OK "See <a href=\"/add/40/2\">40 + 2</a>"
      Suave.Filters.pathScan "/add/%d/%d" (fun (a,b) -> Suave.Successful.OK(string (a + b)))
      Suave.RequestErrors.NOT_FOUND "Found no handlers" ]


// APPLICATION NO.3
type Weather = FSharp.Data.XmlProvider<"http://www.yr.no/place/Norway/Telemark/Sauherad/Gvarv/forecast.xml">
let weatherInPrague = Weather.Load("http://www.yr.no/place/Czech_Republic/Prague/Prague/forecast.xml")
open FSharp.Data
open System.Xml.Linq
let summarizeWeather (w : Weather.Weatherdata) = 
  let place = w.Location.Name + ", " + weatherInPrague.Location.Country
  let list = w.Forecast.Tabular.Times |> Array.toList
  let first = (list |> List.head)
  let temperature = first.Temperature.Value
  let units = first.Temperature.Unit
  sprintf "There is %d%s in %s." temperature units place
let getCzechWeather area town =
  let url = sprintf "http://www.yr.no/place/Czech_Republic/%s/%s/forecast.xml" area town
  let weather = Weather.Load(url)
  weather |> summarizeWeather

let app_3 : WebPart =
  choose
    [ Suave.Filters.path "/" >=> Suave.Successful.OK "Root path";
      Suave.Filters.pathScan "/%s/%s" (fun (a, b) -> Suave.Successful.OK(getCzechWeather a b));
      Suave.RequestErrors.NOT_FOUND "Found no handlers"
    ]


/// Helper function that returns nice HTML page with title & body
let simplePage title body = 
  File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "web/blank.html"))
    .Replace("[TITLE]", title).Replace("[BODY]", body)


// This is how you start web server.
// This way it blocks F# console.
startWebServer defaultConfig okWebPart

// This is how you start web server that does not block F# interactive
// and is possible to cancel.
// Taken from https://github.com/tpetricek/Dojo-Suave-FsHome/blob/master/app.fsx
let config = {Suave.Web.defaultConfig with homeFolder = Some __SOURCE_DIRECTORY__}
let _, task = startWebServerAsync config app_3
let cts = new System.Threading.CancellationTokenSource()
Async.Start(task, cts.Token)
// This is how you stop the server
cts.Cancel()
// This is how I see if the cancelation is requested
cts.Token.IsCancellationRequested


choose
