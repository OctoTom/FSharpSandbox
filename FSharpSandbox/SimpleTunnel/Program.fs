namespace SimpleTunnel

open System
open FSharp.Charting
open System.Windows.Forms
open LogModule

module Program = 
  // [<EntryPoint>]
  [<EntryPoint;STAThread>] // STAThread attribute is needed to invoke charts (FSharp.Charting).
  let main argv =
    //Model2d3d.Tests.test04()
    //Model2d3d.Tests.test05()
    //Model2d3d.Tests.test06()
    //Model2d3d.Tests.test07()
    Model2d3d.Tests.test08()
    printfn "%A" logWriter
    
    System.Console.ReadLine() |> ignore // Wait for key
    0 // return an integer exit code
