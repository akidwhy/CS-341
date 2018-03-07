//
// F# program to analyze Divvy daily ride data.
//
// <<Abdullah Kidwai>>
// U. of Illinois, Chicago
// CS 341, Spring 2018
// Project #04
//

#light

let ParseLine (line:string) = 
  let tokens = line.Split(',')
  let ints = Array.map System.Int32.Parse tokens
  Array.toList ints

let rec ParseInput lines = 
  let rides = Seq.map ParseLine lines
  Seq.toList rides


let rec NumberOfMen x ridedata = 
  match ridedata with
  | [] -> 0
  | hd::tl when (hd <> 1 && hd <> 2) -> NumberOfMen x tl
  | hd::tl when (hd = x) -> 1 + (NumberOfMen x tl)
  | hd::tl -> NumberOfMen x tl


let rec NumberOfWomen x ridedata = 
  match ridedata with
  | [] -> 0
  | hd::tl when (hd <> 1 && hd <> 2) -> NumberOfWomen x tl
  | hd::tl when (hd = x) -> 1 + (NumberOfWomen x tl)
  | hd::tl -> NumberOfWomen x tl


let rec RideTime currentSum ridedata = 
  match ridedata with
  | [] -> currentSum
  | hd::tl  when ((hd <> 1 && hd <> 2) && (tl <> []) && ((tl.Head) = 1 || (tl.Head) = 2)) -> RideTime (currentSum + hd) tl
  | hd::tl -> RideTime currentSum tl

let sumOfRideTime L = 
  RideTime 0 L

let rec avgAgeofMale ridedata =
  match ridedata with 
  | [] -> 0
  | hd::tl when (hd = 1) -> tl.Head + avgAgeofMale tl
  | hd::tl -> avgAgeofMale tl


let rec AgeOfFemale ridedata =
  match ridedata with
  | [] -> 0
  | hd::tl when (hd = 2) -> tl.Head + AgeOfFemale tl
  | hd::tl -> AgeOfFemale tl


[<EntryPoint>]
let main argv =
  //
  // input file name, then input divvy ride data and build
  // a list of lists --- [ [1308;321;2;1991]; ... ]
  //
  let filename = System.Console.ReadLine()
  let contents = System.IO.File.ReadLines(filename)
  let ridedata = ParseInput contents

  //printfn "%A" ridedata
  let N = List.length ridedata
  printfn "# of rides: %A" N


  let concactedList = List.concat ridedata
  //printfn "# of riders: %A" concactedList

  let men = NumberOfMen 1 concactedList
  //printfn "Number of Male riders: %A" men

  let women = NumberOfWomen 2 concactedList
  //printfn "Number of Female riders: %A" women

  let percentMen = (float(men)/float(N)) * 100.0
  let percentWomen = (float(women)/float(N)) * 100.0

  printfn "%% of male riders: %A" percentMen
  printfn "%% of female riders: %A" percentWomen

  let totalRideTime = sumOfRideTime concactedList
  let avgRideTimeinMin = (float (totalRideTime/60)/float (N))
  printfn "Avg duration: %A in min" avgRideTimeinMin

  let AgeOfM = avgAgeofMale concactedList
  let avgMaleAge = (((2018.0*float(men)) - float(AgeOfM)) / float(men))
  printfn "Avg age of male riders: %A" avgMaleAge

  let AgeOfW = AgeOfFemale concactedList
  let avgFemaleAge = (((2018.0*float(women)) - float(AgeOfW)) / float(women))
  printfn "Avg age of female riders: %A" avgFemaleAge


  //
  // TODO:
  //
  0 
