
let nu = 0.3 // [-]
let gamma = 20.0 // kN/m3
let e0 = 1.0
let Cc = 0.5 // Podle Fine 0.15-10

let k_d = (1.0+nu)*(1.0-2.0*nu)/(1.0-nu)*gamma/(1.0+e0)/Cc
