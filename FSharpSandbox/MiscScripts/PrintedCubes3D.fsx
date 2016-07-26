// PLA price
let czkPerKg = 500.0

// Open/closed foam microstructure black cubes 50x50x50 mm.
let a = 0.05 // m
let w = 0.02 // kg
let t = 3.0 // h
let volume = a * a * a
let specificWeight = w / volume
let numPerM3 = (1.0 / a)**3.0
let pricePerM3 = specificWeight * czkPerKg
let hoursPerM3 = numPerM3 * t
let daysPerM3 = hoursPerM3 / 24.0


let printedVolume = 0.03 * 0.03 * 0.02 // m3
let time = 6.5 // min
let timePerM3 = time / printedVolume / 60.0 / 24.0

let l = 0.25 // m
let r = 0.003 / 2.0 // m
let V = 3.14 * r * r * l // m3
let rho = 1250.0 // kg/m3
let mFilament = rho * V // kg

let mCube = 0.002 // kg
let czkForKg = 500.0
let priceCube = mCube * czkForKg

// Cube size
let a = 0.04 // m
let numToM3 = (1.0 / a)**3.0
let priceM3 = numToM3 * priceCube

let load = 2.2 // kg
let force = 22.0 // N
let area = a**2.0
let stress = force / area


