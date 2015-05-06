// Function factory that returns displacement field for given geometry and nodal displacements
// Coordinate system is local to the beam. The origin is at node 0 and x-axis correspond to axis of the beam.
let getDispField (L, dy, dz) nodalDisp0 nodalDisp1 =   
  // Decompose the lists containing nodal displacements
  let [u0; v0; w0; phix0; phiy0; phiz0] = nodalDisp0
  let [u1; v1; w1; phix1; phiy1; phiz1] = nodalDisp1
  // Displacement field - function that returns displacements u, v and w for fiven point x, y, z.
  let dispField (x, y, z) =
    if (x < 0.0 || x > L || y < -dy/2.0 || y > dy/2.0 || z < -dz/2.0 || z > dz/2.0) then
      failwith "Point is outside the beam's volume."
    // Local coordinate r randes from 0 to 1
    let r = x / L
    // Linear shape functions. Local coordinate r = 0 at node 0 and  r = 1 at notde 1.
    let H0 = 1.0 - r
    let H1 = r
    // Define power operator
    let pow x y = System.Math.Pow(x, y)
    // Cubic shape functions. Local coordinate r ranges from 0 to 1.
    let H2 = 1.0 - 3.0 * pow r 2.0 + 2.0 * pow r 3.0
    let H3 = 3.0 * pow r 2.0 - 2.0 * pow r 3.0
    let H4 = L * (r - 2.0 * pow r 2.0 + pow r 3.0)
    let H5 = L * (-pow r 2.0 + pow r 3.0) 
    // Derivatives of the cubic shape function 
    let dH2dx = 6.0 / L * (pow r 2.0 - r)
    let dH3dx = -6.0 / L * (pow r 2.0 - r)
    let dH4dx = (1.0 - 4.0 * r + 3.0 * pow r 2.0)
    let dH5dx = (-2.0 * r + 3.0 * pow r 2.0)
    // Rotation of cross section at given r
    let phix = H0 * phix0 + H1 * phix1
    let dv = dH2dx * v0 + dH3dx * v1 + dH4dx * phiz0 + dH5dx * phiz1 // = phiy
    let dw = dH2dx * w0 + dH3dx * w1 - dH4dx * phiy0 - dH5dx * phiy1 // = -phiz
    // Dsiplacements at arbitrary point (x, y, z) in the beam's volume
    let v = H2 * v0 + H3 * v1 + H4 * phiz0 + H5 * phiz1 - phix * z
    let w = H2 * w0 + H3 * w1 - H4 * phiy0 - H5 * phiy1 + phix * y
    let u = H0 * u0 + H1 * u1 - dv * y - dw * z
    [u; v; w]
  dispField

// Tests
#load "c:\Users\Tomas\Scripts\NuGet\LoadPackages.fsx"

let L, dy, dz = 10.0, 0.2, 0.5
let nodalDisp0 = [0.0; 0.0; 0.0; 1.0; 0.001; 0.0;]
let nodalDisp1 = [0.0; 0.0; 0.0; 1.0; 0.0; 0.0;]
let disp = getDispField (L, dy, dz) nodalDisp0 nodalDisp1
let n = 100
let xPoints = [0..n] |> List.map (fun i -> float i / float n * L)
let plotFunc x = (disp (x, 0.1, 0.25)).[2] // Which component of disp to draw
let points = xPoints |> List.map (fun x -> (x, plotFunc x))
FSharp.Charting.Chart.Line(points)
