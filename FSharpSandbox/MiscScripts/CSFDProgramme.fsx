#load @"c:\Users\Tomas\Scripts\Paket\LoadPackages.fsx"
#r @"c:\Users\Tomas\OneDrive\OneSync\Projects\CommonTools\CommonTools\Bin\Release\CommonTools.dll"

/// Downloads the progremme of cinnemas in Prague from the CSFD database.
/// Open the page in browser.
/// Parses the file, sorts the films by year.
/// Prints names and years to the F# interactive console.
/// (Usage from visual studio: select whole text in file and press Alt-Enter.)

let url = @"http://www.csfd.cz/kino/?period=all"
let fileName = @"c:\Users\Tomas\Downloads\ProgramKinCSFD.html"

// Download
let wc = new System.Net.WebClient()
wc.DownloadFile(url, fileName)

// Open URL in default browser and give it focus.
System.Diagnostics.Process.Start(url)

let lines = CommonTools.IO.readLines fileName
let movieLines =
  lines
  |> Seq.map (fun line -> line.Trim())
  |> Seq.filter (fun line -> line.StartsWith("<th>"))
  |> Seq.skip 2 |> Seq.toList

let getNameYear (line: string) =
  let split = line.Split(["<"; ">"] |> List.toArray, System.StringSplitOptions.RemoveEmptyEntries) |> List.ofArray
  split.[2], split.[6]

let action (name, year) = printfn "%s %s" name year
let noDuplicates = movieLines |> List.map getNameYear |> Set.ofList |> Set.toList
noDuplicates |> List.sortBy snd |> List.iter action
