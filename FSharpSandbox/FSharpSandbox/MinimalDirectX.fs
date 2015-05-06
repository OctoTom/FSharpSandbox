module MinimalDirectX

/// Minimal DirectX demo
/// (C) Flying Frog Consultancy Ltd., 2007
/// http://www.ffconsultancy.com

#if INTERACTIVE
#I @"C:\WINDOWS\Microsoft.NET\DirectX for Managed Code\1.0.2902.0";;
#r @"Microsoft.VisualC.dll";;
#r @"Microsoft.DirectX.dll";;
#r @"Microsoft.DirectX.Direct3D.dll";;
#r @"Microsoft.DirectX.Direct3DX.dll";;
#endif

open System
open System.Drawing
open System.Windows.Forms
open Microsoft.DirectX
open Microsoft.DirectX.Direct3D

// Class encapsulating a DirectX viewer
type Viewer = class
  inherit Form
    
  val mutable device : Device
  val render : Device -> unit
  val mutable world : Matrix
  val mutable drag : (Matrix * int * int) option

  new(title, render) as form = {device=null; render=render; world=Matrix.Identity; drag=None} then
    form.SetStyle(Enum.combine [ControlStyles.AllPaintingInWmPaint; ControlStyles.Opaque], true);
    form.Text <- title;
    form.MinimumSize <- form.Size;
    form.Show()

  // Desired parameters for our DirectX device
  member form.present_params =
    let p = new PresentParameters() in
    p.Windowed <- true;
    p.SwapEffect <- SwapEffect.Discard;
    p.EnableAutoDepthStencil <- true;
    p.AutoDepthStencilFormat <- DepthFormat.D16;
    Idioms.ParamArray [p]

  // Recover the state of our device after DirectX has corrupted it
  member form.device_reset() = ()

  // Create a new DirectX device
  member form.make_device() =
    form.device <- new Device(0, DeviceType.Hardware, form, CreateFlags.SoftwareVertexProcessing, form.present_params);
    form.device.add_DeviceReset(new EventHandler(fun _ _ -> form.device_reset()));
    form.device_reset()

  // All painting is done in the Paint callback, which uses our render function
  override form.OnPaint _ =
    if form.device = null then form.make_device();
    try
      form.device.BeginScene();
      form.device.Clear(Enum.combine [ClearFlags.Target; ClearFlags.ZBuffer], Color.Black, 1.f, 0);
      form.device.Transform.World <- form.world;
      form.render form.device;
      form.device.EndScene();
      form.device.Present()
    with _ -> form.make_device()

  // Resizing refeshes the display
  override form.OnResize _ = form.Invalidate()

  // Start a drag if the mouse is pressed
  override form.OnMouseDown e = form.drag <- Some(form.device.Transform.World, e.X, e.Y)

  // Stop a drag if the mouse is released
  override form.OnMouseUp e = form.drag <- None

  // During dragging, alter the world transformation matrix in the device and force a redraw
  override form.OnMouseMove e = match form.drag with
    | Some(world, x, y) ->
        let scale = 5.f / float32(min form.device.Viewport.Width form.device.Viewport.Height) in
        form.world <- world * Matrix.RotationY(float32(x - e.X) * scale) * Matrix.RotationX(float32(y - e.Y) * scale);
        form.Invalidate()
    | None -> ()

  // Pressing Escape closes the form
  override form.OnKeyPress e = if int_of_char e.KeyChar = Enum.to_int Keys.Escape then form.Close()

  // Quit this application when the form is closed
  override form.OnClosed(e : EventArgs) = Application.Exit()
end

// Render the scene
let render (device : Device) =
  // Add a light
  device.RenderState.SpecularEnable <- true;
  device.Lights.Item(0).Specular <- Color.White;
  device.Lights.Item(0).Direction <- new Vector3(-1.f, -1.f, 2.f);
  device.Lights.Item(0).Update();
  device.Lights.Item(0).Enabled <- true;
  
  // Set the perspective and view transformation matrices
  let aspect = float32 device.Viewport.Width / float32 device.Viewport.Height in
  device.Transform.Projection <- Matrix.PerspectiveFovLH(0.8f, aspect, 0.1f, 10.f);
  device.Transform.View <- Matrix.LookAtLH(new Vector3(0.f, 0.f, -4.f), new Vector3(0.f, 0.f, 0.f), new Vector3(0.f, 1.f, 0.f));
  
  // Use a red material with white specular highlights
  let mutable material = new Material() in
  material.Diffuse <- Color.Red;
  material.Specular <- Color.White;
  material.SpecularSharpness <- 32.f;
  device.Material <- material;
  
  // Render the built-in teapot mesh, making sure it is deallocated immediately
  Idioms.using (Mesh.Teapot(device)) (fun teapot -> teapot.DrawSubset(0))

// Create a window containing a DirectX widget that uses this renderer
let form = new Viewer("Teapot demo", render)

// Loop indefinitely while the form is open
do Application.Run()