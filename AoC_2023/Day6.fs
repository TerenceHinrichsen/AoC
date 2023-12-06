module Day6

   open AdventOfCode
   open System
   open System.Text.RegularExpressions
   open FsToolkit.ErrorHandling

   let sampleInput = [
         "Time:      7  15   30"
         "Distance:  9  40  200"
      ]
   let input =
      TextFileReader.readFileContents "../../../Day6Input.txt"


   type RaceResult = {
      time: int
      distance: int
   }

   type RaceResult2 = {
      time2 : int64
      distance2 : int64
   }

   let getRaceResultFromInput (input: List<string>) =
      let timeRow = input |> List.head
      let distanceRow = input |> List.tail |> List.head

      let times =
         timeRow
         |> fun x -> x.Split(" ")
         |> Array.rev
         |> Array.filter (fun x -> x <> "")
         |> Array.take 3

      let distances =
         distanceRow
         |> fun x -> x.Split(" ")
         |> Array.rev
         |> Array.filter (fun x -> x <> "")
         |> Array.take 3

      [0..2]
      |> List.map (fun count ->
         let time = times[count]
         let distance = distances[count]
         {
            time =  time |> int
            distance = distance |> int
         }
      )

   let getSingleResultFromInput (input: List<string>)  =
      let timeRow = input |> List.head
      let distanceRow = input |> List.tail |> List.head

      printfn $"Time row: {timeRow}"
      printfn $"Distance row: {distanceRow}"

      let times =
         timeRow
         |> fun x -> x.Split(" ")
         |> Array.rev
         |> Array.filter (fun x -> x <> "")
         |> Array.take 4
         |> Array.toList

      printfn $"Times: {times}"
      let distances =
         distanceRow
         |> fun x -> x.Split(" ")
         |> Array.rev
         |> Array.filter (fun x -> x <> "")
         |> Array.take 4
         |> Array.toList

      printfn $"Distances: {distances}"

      let time =
         times
         |> List.rev
         |> String.concat ""
         |> fun x -> x |> Int64.Parse
         |> fun x -> printfn $"Time : {x}"; x

      let distance =
         distances
         |> List.rev
         |> String.concat ""
         |> fun x -> x |> Int64.Parse
         |> fun x -> printfn $"Distance : {x}"; x

      printfn $"Distance: {distance}"
      let result=
         {
            time2 = time
            distance2 = distance
         }
      printfn $"Single result {result}"
      result


   let calculateWinCombinations (raceResult: RaceResult) =
      printfn $"Calculating: {raceResult}"
      let possibleWins =
         seq [1..raceResult.time]
         |> Seq.map (fun speed -> speed * (raceResult.time - speed) )
         |> Seq.filter (fun x -> x > raceResult.distance)

      let numberOfCombinations =
         possibleWins
         |> Seq.length
      numberOfCombinations

   let calculateWinCombinations2 (raceResult: RaceResult2) =
      printfn $"Calculating: {raceResult}"

      let possibleWins =
         seq [1L..raceResult.time2]
         |> Seq.map (fun speed -> speed * (raceResult.time2 - speed) )
         |> Seq.filter (fun x -> x > raceResult.distance2)

      let numberOfCombinations =
         possibleWins
         |> Seq.length
      numberOfCombinations

   let parseInputToAnswer (input: List<string>) =
//      input
//      |> getRaceResultFromInput
//      |> List.rev
//      |> List.map calculateWinCombinations
//      |> List.fold (*) 1
//
//      |> fun x -> printfn $"Part 1: {x}"

      input
      |> getSingleResultFromInput
      |> calculateWinCombinations2
      |> fun x -> printfn $"Part 2: {x}"


   let run = parseInputToAnswer input
