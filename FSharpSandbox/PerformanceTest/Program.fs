// This file calls different tests.
module Program =
  open ToolsLibrary
  open ToolsLibrary.Random // Module that contains the infinite sequences with random elements.

  let toolsLibraryTest () =
    let mySequence = randomFloatSequence (Random.RandomFloatFromTo(10.0, 15.0))
    printfn "Function toolsLibraryTest() called."
    printfn "First 20 elements: %A" (mySequence |> Seq.take 20 |> Seq.toList)


  [<EntryPoint>]
  let main argv = 
    toolsLibraryTest()
    //TailRecursion.Test_TailRecursion |> ignore
    System.Console.ReadLine() |> ignore
    0 // return an integer exit code
