#load @"c:\Users\Tomas\Scripts\Paket\References.fsx"

let names = ["Clay"; "Sand"]

let getInputFilePath name = @"c:\Users\Tomas\Downloads\TabulkyZeminyCalib-" + name + ".csv"
let getOutputFilePath name = @"c:\Users\Tomas\Downloads\" + name + ".txt"


let convertCSVFile name = 
  let inputPath = getInputFilePath name
  let outputPath = getOutputFilePath name
  let lines = CommonTools.IO.readLines inputPath |> Seq.toList
  let formatLine line = 
    let splitLine line = line |> CommonTools.String.split  [","]
    let formatValues values =
      let formatSingleValue s = "<td>" + s + "</td>"
      values |> List.map formatSingleValue
    line |> splitLine |> formatValues
  lines |> List.map formatLine |> List.collect id |> CommonTools.IO.writeLines outputPath

names |> List.iter convertCSVFile