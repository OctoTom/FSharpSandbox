#load @"c:\Users\Tomas\Scripts\NuGet\LoadPackages.fsx"

open Deedle
open System.IO

let ghd binWidth (data:float seq) =
  data |> Seq.countBy (fun x -> round(x / binWidth)) |> Seq.map (fun (a, b) -> (a * binWidth, b)) |> Seq.sortBy fst

let getFrame (path:string) =
  // Function to test if the line is relevant. Excludes empty lines and commented line starting with '#'.
  let isRelevant (line:string) =
    (line.StartsWith("#") || System.String.IsNullOrWhiteSpace(line)) |> not
  let stream = new MemoryStream()
  let sw = new StreamWriter(stream)
  use sr = new StreamReader(path)
  let mutable line = ""
  while (line <- sr.ReadLine(); line <> null) do
    if isRelevant line then sw.WriteLine(line)   
  sw.Flush()
  stream.Position <- 0L // Int64 literal
  let frame = Deedle.Frame.ReadCsv(stream)
  stream.Close()
  sw.Close()
  frame

let frame = getFrame @"c:\Program Files\CmdStan\output.csv"  
let sqn = frame.["myParam"].GetAllObservations() |> Seq.map (fun x -> x.Value.Value)
let length = sqn |> Seq.length
let min, max = Seq.min sqn, Seq.max sqn
let hist = sqn |> ghd 0.05 |> Seq.toList

FSharp.Charting.Chart.Column(hist)
RProvider.graphics.R.hist(sqn)

open  FSharp.Data
//type StanOutput = CsvProvider<"c:/Program Files/CmdStan/output.csv">

// Charting with R from F#
// http://bluemountaincapital.github.io/FSharpRProvider/Charts-QuickStart.html

open System
open RDotNet
open RProvider
open RProvider.graphics   

// ==== Example charting ====
let widgets = [ 3; 8; 12; 15; 19; 18; 18; 20; ]
let sprockets = [ 5; 4; 6; 7; 12; 9; 5; 6; ]
R.plot(widgets)
R.plot(widgets, sprockets)
R.barplot(widgets)
R.hist(sprockets)
R.pie(widgets)


// ==== Example of Exporting Chart ====
// Required package to save charts
open RProvider.grDevices
// Create path to an image testimage.png on the Desktop
let desktop = Environment.GetFolderPath(Environment.SpecialFolder.Desktop)  
let path = desktop + @"\testimage.png"
// Open the device and create the file as a png.
// R.bmp, R.jpeg, R.pdf, ... will generate other formats.
R.png(filename=path, height=200, width=300, bg="white")
// Create the chart into the file
R.barplot(widgets)
// Close the device once the chart is complete
R.dev_off ()

// ==== Advanced Charts Options ====
R.barplot(widgets)
R.title(main="Widgets", xlab="Period", ylab="Quantity")

// Named Parameters
R.plot(
    namedParams [   
        "x", box widgets; 
        "type", box "o"; 
        "col", box "blue";
        "ylim", box [0; 25] ])

R.lines(
    namedParams [   
        "x", box sprockets; 
        "type", box "o"; 
        "pch", box 22;
        "lty", box 2;
        "col", box "red" ])

// Or
namedParams [   
    "x", box widgets; 
    "type", box "o"; 
    "col", box "blue";
    "ylim", box [0; 25] ]
|> R.plot

namedParams [   
    "x", box sprockets; 
    "type", box "o"; 
    "pch", box 22;
    "lty", box 2;
    "col", box "red" ]
|> R.lines
