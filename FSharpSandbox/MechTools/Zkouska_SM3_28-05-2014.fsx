// Zkouska SM3 28. kvetna 2014

// Silova metoda// 
let l1 = 4.0
let l2 = 3.0

let h = 0.25
let b = 0.15
let I = 1.0 / 12.0 * b * h**3.0

let E = 25.0e6
let alpha = 1.2e-5
let EI = E * I

// Zatizeni
let f = 6.0
let th = 35.0
let td = 15.0
let tref = 0.0

// Vnitrni sily
let Mf = 1.0 / 8.0 * f * l1**2.0
let M1 = 1.0
let M2 = l2
let N1 = 1.0 / l1
let N2 = l2 / l1

// Delty
let del11 = 1.0 / EI * (1.0 / 3.0 * M1 * M1 * l1 + M1 * M1 * l2)
let del12 = 1.0 / EI * (1.0 / 3.0 * M1 * M2 * l1 + 1.0 / 2.0 * M1 * M2 * l2)
let del22 = 1.0 / EI * (1.0 / 3.0 * M2 * M2 * (l1 + l2))
let del1f = 1.0 / EI * (1.0 / 3.0 * M1 * Mf * l1)
let del2f = 1.0 / EI * (1.0 / 3.0 * M2 * Mf * l1)
let del1t = N1 * alpha * (td + th) / 2.0 * l2 + M1 * alpha * (td - th) / h * l2
let del2t = N2 * alpha * (td + th) / 2.0 * l2 + 1.0 / 2.0 * M2 * alpha * (td - th) / h * l2

let RHS1 = -del1f - del1t
let RHS2 = -del2f - del2t

// Solution of system of linear equations
let X2 = (RHS2 - RHS1 * del12 / del11) / (del22 - del12 * del12 / del11)
let X1 = (RHS1 - del12 * X2) / del11

// Verify solution
let lhs1 = del11 * X1 + del12 * X2
let lhs2 = del12 * X1 + del22 * X2

// Reakce
let Ax = -X2
let Cz = (-l2 * X2 - X1 - f * l1**2.0 / 2.0) / l1
let Az = -(l2 * Ax - X1 + f * l1**2.0 / 2.0) / l1
let check = Az + Cz + l1 * f

// Vykresleni momentu
let M_roh = X1 - l2 * Ax
let M_roh2 = -l1 * Cz - f * l1**2.0 / 2.0
let x_max = -Az / f
let M_max = M_roh - Az * x_max - 1.0 / 2.0 * f * x_max**2.0
let M_max2 = -Cz * (l1 - x_max) - f * (l1 - x_max)**2.0 / 2.0


// Deformacni metoda //
// Reference a dynamic library
#r "../packages/MathNet.Numerics.2.6.1/lib/net40/MathNet.Numerics.dll"
#r "../packages/MathNet.Numerics.FSharp.2.6.0/lib/net40/MathNet.Numerics.FSharp.dll"

/// Open often used namespaces
open MathNet.Numerics.LinearAlgebra.Double

/// Open often used namespaces
open MathNet.Numerics.LinearAlgebra.Double
let CreateMatrix array = DenseMatrix.ofArray2(array2D array)

// DEFORMACNI METODA
let EI = 20000.0

// Rozmery
let l1 = 4.0 // vodorovne
let l2 = 4.0 // svisle
let l3 = 1.0 // pulka konzoly

// Zatizeni
let F = 10.0
let f = 5.0
let wa = 0.003

// Tuhosti prutu
let kab = 3.0 / 4.0 * 2.0 * EI / l2
let kbd = 3.0 / 4.0 * 2.0 * EI / l1
let kcd = 2.0 * EI / l2

let Mde = F * l3

// Nezname jsou phi_d a u

// Koncove momenty
let Mab u = 0.0 + kab * (2.0 * u / l2)
let Zab u = 0.0 - kab / l2 * (2.0 * u / l2)
let Zba u = 0.0 + kab / l2 * (2.0 * u / l2)

let Mdb phi_d = -1.0 / 8.0 * f * l1**2.0 + kbd * (2.0 * phi_d + 2.0 * (-wa) / l1)
let Zbd phi_d = -3.0 / 8.0 * f * l1 - kbd / l1 * (2.0 * phi_d + 2.0 * (-wa) / l1)
let Zdb phi_d = -5.0 / 8.0 * f * l1 + kbd / l1 * (2.0 * phi_d + 2.0 * (-wa) / l1)

let Mcd phi_d u = 0.0 + kcd * (1.0 * phi_d + 3.0 * u / l2)
let Mdc phi_d u = 0.0 + kcd * (2.0 * phi_d + 3.0 * u / l2)
let Zcd phi_d u = 0.0 - kcd / l2 * (3.0 * phi_d + 6.0 * u / l2)
let Zdc phi_d u = 0.0 + kcd / l2 * (3.0 * phi_d + 6.0 * u / l2)

// Polynomy
let rce_phi phi u = Mdb phi + Mdc phi u + Mde
let rce_u   phi u = Zba u + Zdc phi u  

// Extract stiffness matrix elements
let K11 = rce_phi 1.0 0.0 - rce_phi 0.0 0.0
let K12 = rce_phi 0.0 1.0 - rce_phi 0.0 0.0
let K21 = rce_u 1.0 0.0 - rce_u 0.0 0.0
let K22 = rce_u 0.0 1.0 - rce_u 0.0 0.0
// Extract right hand side
let b1 = rce_phi 0.0 0.0
let b2 = rce_u 0.0 0.0

// Matice tuhosti
let K = CreateMatrix [[K11; K12]; [K21; K22]]
// Prava strana
let bb = CreateMatrix [[-b1]; [-b2]]
// Reseni soustavy rovnic
let r = K.Inverse() * bb

let phi_d = r.At(0, 0)
let u = r.At(1, 0)

let check1 = rce_phi phi_d u
let check2 = rce_u phi_d u

let Mab_ = Mab u
let Zab_ = Zab u
let Zba_ = Zba u

let Mdb_ = Mdb phi_d
let Zbd_ = Zbd phi_d
let Zdb_ = Zdb phi_d

let Mcd_ = Mcd phi_d u
let Mdc_ = Mdc phi_d u
let Zcd_ = Zcd phi_d u
let Zdc_ = Zdc phi_d u

// Extrem
let x_max = -Zbd_ / f
let M_max = -Zbd_ * x_max - f * x_max**2.0 / 2.0
