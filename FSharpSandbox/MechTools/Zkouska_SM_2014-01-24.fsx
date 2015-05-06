// Reference a dynamic library
#r "../packages/MathNet.Numerics.2.6.1/lib/net40/MathNet.Numerics.dll"
#r "../packages/MathNet.Numerics.FSharp.2.6.0/lib/net40/MathNet.Numerics.FSharp.dll"

/// Open often used namespaces
open MathNet.Numerics.LinearAlgebra.Double

/// Open often used namespaces
open MathNet.Numerics.LinearAlgebra.Double
let CreateMatrix array = DenseMatrix.ofArray2(array2D array)

// DEFORMACNI METODA

// Prurez
let b = 0.15
let h = 0.2
let I = 1.0 / 12.0 * b * h**3.0

// Material
let E = 20.0e6
let alpha = 1.0e-6
let EI = E * I

// Rozmery
let l = 4.0

// Zatizeni
let F = -12.0
let M = 6.0
let f = 9.0
let t = 20.0

// Protazeni vodorovneho prutu od teploty
let delta_l = alpha * t * l

// Tuhosti prutu
let kab = 3.0 / 4.0 * 2.0 * EI / l
let kbd = 2.0 * EI / l

// Koncove momenty
let Mba phi_b = 0.0 + kab * (2.0 * phi_b + 2.0 * (-delta_l) / l)
let Zba phi_b = 0.0 + kab / l * (2.0 * phi_b + 2.0 * (-delta_l) / l)
let Zab phi_b = 0.0 - kab / l * (2.0 * phi_b + 2.0 * (-delta_l) / l)

let Mbd phi_b w_b = 1.0 / 12.0 * f * l**2.0 + kab * (2.0 * phi_b + 3.0 * (-w_b) / l)
let Mdb phi_b w_b = -1.0 / 12.0 * f * l**2.0 + kab * (1.0 * phi_b + 3.0 * (-w_b) / l)
let Zbd phi_b w_b = -1.0 / 2.0 * f * l - kab / l * (3.0 * phi_b + 3.0 * (-w_b) / l)
let Zdb phi_b w_b = -1.0 / 2.0 * f * l + kab / l * (3.0 * phi_b + 3.0 * (-w_b) / l)

let rce_phi phi w = Mbd phi w + Mba phi - M 
let rce_w phi w = Zbd phi w - F 

// Extract stiffness matrix elements
let K11 = rce_phi 1.0 0.0 - rce_phi 0.0 0.0
let K12 = rce_phi 0.0 1.0 - rce_phi 0.0 0.0
let K21 = rce_w 1.0 0.0 - rce_w 0.0 0.0
let K22 = rce_w 0.0 1.0 - rce_w 0.0 0.0
// Extract right hand side
let b1 = rce_phi 0.0 0.0
let b2 = rce_w 0.0 0.0

// Matice tuhosti
let K = CreateMatrix [[K11; K12]; [K21; K22]]
// Prava strana
let bb = CreateMatrix [[-b1]; [-b2]]
// Reseni soustavy rovnic
let r = K.Inverse() * bb

let phi_b = r.At(0, 0)
let w_b = r.At(1, 0)

let Mba_ = Mba phi_b 
let Mbd_ = Mbd phi_b w_b
let Mdb_ = Mdb phi_b w_b

let Zab_ = Zab phi_b
let Zba_ = Zba phi_b
let Zbd_ = Zbd phi_b w_b
let Zdb_ = Zdb phi_b w_b


// Silova metoda

let delta_11 = 1.0 / EI * 2.0 * (2.0**3.0 / 3.0 + 16.0)
let delta_22 = 1.0 / EI * 2.0 * (4.0**3.0 / 3.0)
let delta_12 = 1.0 / EI * 2.0 * (16.0)
let delta_2r = 0.06

let C = CreateMatrix([[delta_11; delta_12]; [delta_12; delta_22]])
let rhs = CreateMatrix([[0.0]; [-delta_2r]])
let result = C.Inverse() * rhs
