let E = 12.0e6
let F = 150.0
let f = 12.0
let L = 3.0

let A = 0.12 * 0.36 + 0.36 * 0.12
let zT = (0.12 * 0.36 * 0.36 / 2.0 + 0.36 * 0.12 * (0.36 + 0.12 / 2.0)) / A

let Iy = 1.0 / 12.0 * 0.36**3.0 * 0.12 + 1.0 / 12.0 * 0.12**3.0 * 0.36 + 0.12 * 0.36 * (0.36 / 2.0 - zT)**2.0 + 0.36 * 0.12 * (0.36 + 0.12 / 2.0 - zT)**2.0
let Iz = 1.0 / 12.0 * 0.12**3.0 * 0.36 + 1.0 / 12.0 * 0.36**3.0 * 0.12

let Nx = - F
let Qy = 0.0
let Qz = f * L
let Mx = 0.0
let My = -1.0 / 2.0 * f * L**2.0 - F * (0.48 - zT)
let Mz = 0.36 / 2.0 * F

let sig_x (y, z) = Nx / A - Mz / Iz * y + My / Iy * z
// Points with extreme sigma x.
let a = sig_x (0.36 / 2.0, 0.48 - zT)
let b = sig_x (-0.12 / 2.0, -zT)
let c = sig_x (-0.36 / 2.0, 0.36 - zT)

let Sy1 = zT**2.0 / 2.0 * 0.12
let Sy2 = 0.36 * 0.12 * (0.36 + 0.12 / 2.0 - zT)
let Sy3 = 0.12 * 0.12 * (0.36 + 0.12 / 2.0 - zT)

let tau S b = Qz * S / b / Iy

let tauxz_1 = tau 0.0 0.12
let tauxz_2 = tau Sy1 0.12
let tauxz_3 = tau Sy2 0.12
let tauxz_4 = tau Sy2 0.36
let tauxz_5 = tau 0.0 0.36

let tauxy_1 = tau 0.0 0.12
let tauxy_2 = tau Sy3 0.12
let tauxy_3 = tau Sy3 0.48
let tauxy_4 = tau 0.0 0.48

let u = Nx * L / 2.0 / E / A 


