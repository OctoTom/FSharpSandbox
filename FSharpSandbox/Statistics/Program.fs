// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open RandomProcess
open System.IO

[<EntryPoint>]
let main argv = 
  printfn "%A" argv
    
  /// Returns lines of file as sequence of strings
  let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
    }
  /// Saves sequence of strings as lines of a given file.
  let writeLines (filePath : string) lines = 
    use outFile = new StreamWriter(filePath)
    let action (s : string) = outFile.WriteLine(s)
    do lines |> Seq.iter action

  /// Random generator
  let rnd = System.Random()
  
      
  // Samples taken from one place. No autocorrelation.
  let numExp = 40 // Number of samples for experimental variability assessment.
  let muExp = 35.0 // Mean value of these samples.
  let varExp = 0.5 // Variance of these samples.

  // Auto-correlated samples taken from distinct places.
  let dx = 0.05 // Spacing between xs points.
  let numSpace = 100 // Number of spatially distributed samples.
  let muSpace = 33.0 // Mean value of the random process.
  let covSpace = 2.0 // Process variance.
  let hc = 0.2 // Process correlation length.

  let points, gridValues = getRandomField (1.0, dx) (1, numSpace) (covFun covSpace hc)

  let ye = List.init numExp (fun _ -> MathNet.Numerics.Distributions.Normal.Sample(rnd, muExp, sqrt varExp))
  let xs = points |> List.map (fun point -> point.[1])
  let ys_true = List.init (gridValues.GetLength(1)) (fun i -> muSpace + gridValues.[0,i])
  let ys_true_uncorr = List.init numSpace (fun _ -> MathNet.Numerics.Distributions.Normal.Sample(rnd, muSpace, sqrt covSpace))
  let ys_error = List.init numSpace (fun _ -> MathNet.Numerics.Distributions.Normal.Sample(rnd, 0.0, sqrt varExp))
  let ys = List.map2 (+) ys_true ys_error
  let ys_uncorr = List.map2 (+) ys_true_uncorr ys_error

  // Save values to file
  ye |> List.map (sprintf "%A") |> writeLines @"c:\Users\Tomas\Sync\R\SandProfileNew\ye.txt"
  xs |> List.map (sprintf "%A") |> writeLines @"c:\Users\Tomas\Sync\R\SandProfileNew\xs.txt"
  ys_true |> List.map (sprintf "%A") |> writeLines @"c:\Users\Tomas\Sync\R\SandProfileNew\ys_true.txt"
  ys_true_uncorr |> List.map (sprintf "%A") |> writeLines @"c:\Users\Tomas\Sync\R\SandProfileNew\ys_true_uncorr.txt"
  ys |> List.map (sprintf "%A") |> writeLines @"c:\Users\Tomas\Sync\R\SandProfileNew\ys.txt"
  ys_uncorr |> List.map (sprintf "%A") |> writeLines @"c:\Users\Tomas\Sync\R\SandProfileNew\ys_uncorr.txt"

  // let res = getRandomField (10.0, 10.0) (50, 1) (covFun 10.0 50.0) |> snd |> saveGridValuesAsBitmap @"C:\Users\Tomas\Desktop\TestImage2.bmp"

  0 // return an integer exit code
