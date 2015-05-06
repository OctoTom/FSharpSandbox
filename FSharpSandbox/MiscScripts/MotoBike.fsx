module MotoBike

let otackyMotoru = 4500.0 / 60.0

let nexus = (0.527, 1.615)
let pastorekNexus = (16.0, 23.0)

let prevodovka x = x / 10.0
let prumerKola = 0.7
let pastorekNaPrevodovce = 16.0
let pastorekNaKole = 23.0
let prevodRetezem x = x * pastorekNaPrevodovce / pastorekNaKole

let otackyKola = otackyMotoru |> prevodovka |> prevodRetezem |> (*) (snd nexus)

let obvodKola = 3.14 * prumerKola
let rychlost = otackyKola * obvodKola

let rychlostKmH = rychlost * 3.6

let kazeta = (11.0, 32.0)