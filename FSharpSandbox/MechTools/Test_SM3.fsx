// 1. Zapoctovy test SM3
let l1 = 1.0
let l2 = 4.0
let l3 = 2.0
let l4 = 3.0

let h = 0.25
let b = 0.1
let I = 1.0 / 12.0 * b * h**3.0

let E = 20e6
let alpha = 12e-6
let EI = E * I

let f = 8.0
let wc = 0.01
let th = 50.0
let td = 20.0

let M1 = - 1.0 / 2.0 * l1**2.0 * f
let M2 = 1.0 / 8.0 * l2**2.0 * f

let del11 = 1.0 / EI * (1.0 / 3.0 * (l2 + l3))
let del12 = 1.0 / EI * (1.0 / 6.0 * l3)
let del22 = 1.0 / EI * (1.0 / 3.0 * (l3 + l4))
let del1f = 1.0 / EI * (1.0 / 6.0 * M1 * l2 + 1.0 / 3.0 * M2 * l2)
let del2f = 0.0
let del1t = 0.0
let del2t = 1.0 / 2.0 * l4 * alpha * (td - th) / h
let virtR1 = 1.0 / 4.0 + 1.0 / 2.0
let virtR2 = - 1.0 / 2.0
let del1r = -(virtR1 * wc)
let del2r = -(virtR2 * wc)
let del1v = 0.0
let del2v = 0.0

let RHS1 = del1v - del1f - del1t - del1r
let RHS2 = del2v - del2f - del2t - del2r

// Solution of system of linear equations
let X2 = (RHS2 - RHS1 * del12 / del11) / (del22 - del12 * del12 / del11)
let X1 = (RHS1 - del12 * X2) / del11
// Verify solution
let lhs1 = del11 * X1 + del12 * X2
let lhs2 = del12 * X1 + del22 * X2
