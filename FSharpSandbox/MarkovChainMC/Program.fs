module Program

open System.IO
open MatrixModule
open MyDistributions
open MetropolisAlgorithm

let a = MatrixModule.Matrix(array2D[[1.0; 2.0]; [3.0; 4.0]])
let b = MatrixModule.Matrix(array2D[[10.0; 20.0]; [30.0; 40.0]])
let rectangle = MatrixModule.Matrix(array2D[[10.0; 20.0; 30.0]; [30.0; 40.0; 50.0]])
let c = rectangle.Transpose

[<EntryPoint>]
let main argv = 
  printfn "Function main() started."
  printfn "%A" argv
  let numSamples = 1000000
  let samples = samplesND |> Seq.take numSamples |> Seq.toList
  printfn "There were generated %A samples." numSamples
  printfn "Function main() finished. Press enter to close console window."
  System.Console.ReadLine() |> ignore // Wait for enter key
  0 // return an integer exit code
