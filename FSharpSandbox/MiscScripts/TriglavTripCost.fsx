// -- Triglav trip cost --
// Values provided by Marťa
// Model created by Tom
[<Measure>] type km
[<Measure>] type litre
[<Measure>] type czk
[<Measure>] type eur
let eurToCzk x = 27.5<czk/eur> * x 
let distance = 1550.0<km>
let consumption = 6.1<litre> / 100.0<km>
let petrolUnitPrice = 37.0<czk> / 1.0<litre>
let tollStickerPrice = 8.5<eur> |> eurToCzk 
let tollPrice = (11.0<eur> + 5.0<eur>) |> eurToCzk
let parkingPrice = 3.5<eur> |> eurToCzk
let numOfPersons = 3
let petrolTotalPrice = distance * consumption * petrolUnitPrice
let totalPrice = petrolTotalPrice + tollStickerPrice + tollPrice + parkingPrice
let pricePerPerson = totalPrice / float numOfPersons
  
