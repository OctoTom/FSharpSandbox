// open System // System.STAThread  
open System.IO
open System.Drawing
open System.Windows.Forms

let convertDataRow (line:string) =
  let items = line.Split([|','|]) |> Seq.toList
  match items with
  | first::second::[] -> (items.[0], int32 items.[1])
  | _ -> failwithf "Bad data"
let res1 = convertDataRow "Australia/Oceania,30"
let processLines lines =
  let rec processLinesRec lines = 
    match lines with
    | [] -> []
    | current :: remaining ->
      let parsedLine = convertDataRow current
      let parsedRest = processLinesRec remaining
      parsedLine :: parsedRest
  processLinesRec lines
let res2 = processLines ["Asia,3634"; "Australia/Oceania,30"; "Africa,767"]
let calculateSum rows =
  let rec calculateSumRec rows =
    match rows with
    | [] -> 0
    | (_, value) :: tail -> value + calculateSumRec tail
  calculateSumRec rows
let res3 = res2 |> calculateSum
let allLinesInFile = File.ReadAllLines(@"C:\Users\Tomas\SkyDrive\Tech\Projects\FSharpSandbox\RealWorldFuncPieChart\Data.csv") |> Seq.toList
let parsedLines = allLinesInFile |> processLines
let mySum = calculateSum parsedLines

// UI form
let mainForm = new Form(Width = 620, Height = 450, Text = "My Pie Chart")
let menu = new ToolStrip()
let btnOpen = new ToolStripButton("Open")
let btnSave = new ToolStripButton("Save", Enabled = false)
menu.Items.Add(btnOpen) |> ignore
menu.Items.Add(btnSave) |> ignore
let boxChart =
  new PictureBox(BackColor = Color.White, Dock = DockStyle.Fill, SizeMode = PictureBoxSizeMode.CenterImage)
mainForm.Controls.Add(menu)
mainForm.Controls.Add(boxChart)

// Drawing
let random = System.Random()
let randomBrush () =
  let r, g, b = random.Next(256), random.Next(256), random.Next(256)
  new SolidBrush(Color.FromArgb(r, g, b))
let drawPieSegment (gr:Graphics) title startAngle occupiedAngle =
  let brush = randomBrush ()
  gr.FillPie(brush, 170, 70, 260, 260, startAngle, occupiedAngle)
  brush.Dispose()
let fnt = new Font("Times New Roman", 11.0f)
let centerX, centerY = 300.0, 200.0
let labelDistance = 150.0
let drawLabel (gr:Graphics) title startAngle angle =
  let lblAngle = float(startAngle + angle/2)
  let ra = System.Math.PI * 2.0 * lblAngle / 360.0
  let x = centerX + labelDistance * cos(ra)
  let y = centerY + labelDistance * sin(ra)
  let size = gr.MeasureString(title, fnt)
  let rc = new PointF(float32(x) - size.Width / 2.0f, float32(y) - size.Height / 2.0f)
  gr.DrawString(title, fnt, Brushes.Black, new RectangleF(rc, size))
let drawStep drawingFunc (gr:Graphics) sum data =
  let rec drawStepRec data angleSoFar =
    match data with
    | [] -> ()
    | [title, _] ->
      let angle = 360 - angleSoFar
      drawingFunc gr title angleSoFar angle
    | (title, value) :: tail ->
      let angle = int ((float value) / sum * 360.0)
      drawingFunc gr title angleSoFar angle
      drawStepRec tail (angleSoFar + angle)
  drawStepRec data 0
let drawChart(file) =
  let lines = List.ofSeq(File.ReadAllLines(file))
  let data = processLines(lines)
  let sum = float(calculateSum(data))
  let pieChart = new Bitmap(600, 400)
  let gr = Graphics.FromImage(pieChart)
  gr.Clear(Color.White)
  drawStep drawPieSegment gr sum data
  drawStep drawLabel gr sum data
  gr.Dispose()
  pieChart
  
// User Interaction
let openAndDrawChart(e) =
  let dlg = new OpenFileDialog(Filter="CSV Files|*.csv")
  if (dlg.ShowDialog() = DialogResult.OK) then
    let pieChart = drawChart(dlg.FileName)
    boxChart.Image <- pieChart
    btnSave.Enabled <- true
let saveDrawing(e) =
  let dlg = new SaveFileDialog(Filter="PNG Files|*.png")
  if (dlg.ShowDialog() = DialogResult.OK) then
    boxChart.Image.Save(dlg.FileName)

// Run the application and show the UI
[<System.STAThread>]
do
  btnOpen.Click.Add(openAndDrawChart)
  btnSave.Click.Add(saveDrawing)
  Application.Run(mainForm)

//[<EntryPoint>]
//let main argv = 
//    printfn "%A" argv
//    0 // return an integer exit code
