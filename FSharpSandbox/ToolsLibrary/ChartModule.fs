namespace ToolsLibrary

open System.Windows.Forms
open FSharp.Charting

module ChartModule =
  type someType = FSharp.Charting.value

  /// Draws chart in separate System.Windows.Form.
  /// Can be called from console application.
  let drawChart chart =
    //let form = new Form(Visible = true, TopMost = true, Width = 700, Height = 500)
    let form = new Form(Visible = true, Width = 700, Height = 500)
    form.Controls.Add(new FSharp.Charting.ChartTypes.ChartControl(chart, Dock = DockStyle.Fill))
    Application.Run(form)

  let drawLine (data:seq<float * float>) =
    let line = Chart.Line data
    drawChart line
    ()

  let drawTimeSeries (data:seq<System.DateTime * float>) =
    let line = Chart.Line data
    drawChart line
    ()

  /// Static class wrapping charting library.
  /// It allows for method overloading.
  type ChartHelper = class
    static member DrawDataSet (data:seq<float * float>, name) =
      let line = Chart.FastLine (data, Name=name)
      drawChart line
    static member DrawDataSet (data:seq<System.DateTime * float>, ?name:string) =
      let line =
        match name with
        | Some(name) -> Chart.FastLine (data, Name=name) |> Chart.WithLegend(true)
        | None -> Chart.FastLine (data)
      drawChart line
    end
