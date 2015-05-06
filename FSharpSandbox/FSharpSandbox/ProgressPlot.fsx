// Displays a chart of progress
open FSharp.Charting

let series1 = [(9.0, 0.0); (12.0, 2.5); (15.0, 2.5); (16.0, 3.0); (16.75, 4.5); (18.0, 4.5); (19.25, 5.5); (20.0, 7.0)]

let series2 = [(9.0, 0.0); (22.0, 8.0)]

let comb = FSharp.Charting.Chart.Combine [Chart.Line(series1, Name="Series1"); Chart.Line(series2, Name="Series2")] 
