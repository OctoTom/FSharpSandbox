// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open RandomProcess

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    

    let res = getRandomField (10.0, 10.0) (20, 20) (covFun 10.0 50.0) |> snd |> saveGridValuesAsBitmap @"C:\Users\Tomas\Desktop\TestImage2.bmp"

    0 // return an integer exit code
