namespace MicroMech

// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

module Program = 
  [<EntryPoint>]
  let main argv = 
    // printfn "%A" Rotation.matrix 

    //Rotation.test01()
    //Rotation.test02()
    Rotation.test03()

    System.Console.ReadKey() |> ignore
    0 // Return an integer exit code
