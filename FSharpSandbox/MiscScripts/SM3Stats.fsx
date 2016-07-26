#load @"c:\Users\Tomas\Scripts\Paket\LoadPackages.fsx"

open System
open FSharp.Data

type CSVFile = FSharp.Data.CsvProvider<"c:\Users\Tomas\Downloads\SM3\Book1.csv">
type CSVFile2 = FSharp.Data.CsvProvider<"c:\Users\Tomas\Downloads\SM3\My.csv">


let csv = CSVFile.Load(@"c:\Users\Tomas\Downloads\SM3\Book1.csv")
csv.Rows |> Seq.length
let withNumber = csv.Rows |> Seq.toList |> List.filter (fun item -> (item.``Personal no.``).Trim().StartsWith("9"))
let isW (item : CSVFile.Row) = item.``Personal no.``.[2] = '5' || item.``Personal no.``.[2] = '6'
let w = withNumber |> List.filter isW
let total = withNumber |> List.length
let totalW = w |> List.length
let totDict = withNumber |> List.map (fun item -> item.``Student no.`` |> int, item) |> dict

let csvMy = CSVFile2.Load(@"c:\Users\Tomas\Downloads\SM3\My.csv")
let myNums = csvMy.Rows |> Seq.map (fun item -> item.``Identifik�tor``) |> Seq.toList

myNums |> List.map (fun id -> totDict.Item(id)) |> List.filter isW |> List.length

// Convenient, functional TryParse wrappers returning option<'a>
// http://www.fssnip.net/2y
let tryParseWith tryParseFunc = tryParseFunc >> function
    | true, v    -> Some v
    | false, _   -> None

let parseDate   = tryParseWith System.DateTime.TryParse
let parseInt    = tryParseWith System.Int32.TryParse
let parseSingle = tryParseWith System.Single.TryParse
let parseDouble = tryParseWith System.Double.TryParse

// Average test
let allRes = csv.Rows |> Seq.map (fun item -> parseDouble item.``1``) |> Seq.toList
let totalAverage = allRes |> List.choose id |> List.average

let myRes = myNums |> List.map (fun id -> totDict.Item(id)) |> List.map (fun item -> parseDouble item.``1``)
let myAverage = myRes |> List.choose id |> List.average

let c a b = MathNet.Numerics.Combinatorics.Combinations(a, b)

let p n = c 53 n * c 73 (40-n) / c 126 40

let probData = [28..40] |> List.sumBy p 
probData * 100.0
