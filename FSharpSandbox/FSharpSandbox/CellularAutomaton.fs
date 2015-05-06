module CellularAutomaton

/// Rule 30 Cellular Automaton
/// (C) Flying Frog Consultancy Ltd., 2007
/// http://www.ffconsultancy.com

open System
open System.Drawing
open System.Windows.Forms

// Format of our bitmap
let format = Imaging.PixelFormat.Format24bppRgb

// Create a bitmap the size of the given rectangle and fill it using
// the function "f"
let bitmap_of f (r : Rectangle) =
  let bitmap = new Bitmap(r.Width, r.Height, format) in
  f bitmap;
  bitmap

// Resize the form by replacing and rerendering the bitmap
let resize f (b : Bitmap ref) (w : #Form) _ =
  b := bitmap_of f w.ClientRectangle;
  w.Invalidate()

// Paint the form by drawing the bitmap
let paint (b : Bitmap ref) (v : #Form) (e : PaintEventArgs) =
  let r = e.ClipRectangle in
  e.Graphics.DrawImage(!b, r, r, GraphicsUnit.Pixel)

// Create a form that visualizes a bitmap that is rendered by the
// function "f"
let make_raster title f =
  let form = new Form(Text=title, Visible=true) in
  form.Size <- new Size(512, 512);
  let bitmap = ref (bitmap_of f form.ClientRectangle) in
  form.Resize.Add(resize f bitmap form);
  form.Paint.Add(paint bitmap form);
  form.KeyDown.Add(fun e -> if e.KeyCode = Keys.Escape then form.Close());
  form

// Two cell states
type cell = A | B

// Wolfram's rule 30
let rule30 a b c = match a, b, c with
  | B, B, _ | B, A, B | A, A, A -> A
  | _ -> B

// Evolve the next generation of our cellular automaton
let rec evolve_aux rule = function
  | a::(b::c::_ as t) -> rule a b c :: evolve_aux rule t
  | [a; b] -> [rule a b A; rule b A A]
  | [a] -> [rule A a A; rule a A A]
  | [] -> [rule A A A]

let evolve rule list = rule A A (List.head list) :: evolve_aux rule (A :: list)

// Convert cell state into a color
let color_of_cell = function A -> Color.White | B -> Color.Black

// Set a pixel in our bitmap to the given color
let set (bitmap : Bitmap) y x c =
  let x = bitmap.Width/2 - y + x in
  if 0<=x && x<bitmap.Width then
    bitmap.SetPixel(x, y, color_of_cell c)

// Draw generations of our cellular automaton into a bitmap
let draw rule bitmap =
  let aux gen y =
    List.iteri (set bitmap y) gen;
    evolve rule gen in
  ignore (Seq.fold aux [B] {0 .. bitmap.Height-1})

// Visualize the rule 30 cellular automaton as a bitmap
let form = make_raster "Rule 30" (draw rule30)

// This demo can be run from an F# interactive session or as a
// compiled .NET application. In the latter case, the application
// blocks until the form is closed before exiting.
#if COMPILED
do Application.Run(form)
#endif