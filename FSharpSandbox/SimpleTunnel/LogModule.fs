module LogModule
open System.Text // StringBuilder
open System.IO // StringWriter
open Microsoft.FSharp.Core.Printf // bprintf myStringBuilder "Some Text with param %A" param

let logWriter = new StringWriter()
let log x =
  fprintfn logWriter x
