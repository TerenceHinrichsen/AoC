module Day8

   open AdventOfCode
   open System
   open System.Text.RegularExpressions
   open FsToolkit.ErrorHandling

   let sampleInput = [
         "LLR"
         ""
         "AAA = (BBB, BBB)"
         "BBB = (AAA, ZZZ)"
         "ZZZ = (ZZZ, ZZZ)"
      ]

   let sampleInput2 = [
      "LR"
      ""
      "11A = (11B, XXX)"
      "11B = (XXX, 11Z)"
      "11Z = (11B, XXX)"
      "22A = (22B, XXX)"
      "22B = (22C, 22C)"
      "22C = (22Z, 22Z)"
      "22Z = (22B, 22B)"
      "XXX = (XXX, XXX)"

   ]
   let input =
      TextFileReader.readFileContents "../../../Day8Input.txt"

   let parseInputToAnswer (input: List<string>) =
      let directions =
         input |> List.head
      printfn $"Directions: {directions}"
      let myMap =
         input
         |> List.tail
         |> List.filter (fun x -> x <> "")
         |> List.map (fun x ->
            let coordinate = x.Substring(0,3)
            let leftRight = x.Substring(7,3), x.Substring(12,3)
            coordinate, leftRight
            )
         |> Map.ofList

      let navigateDirections (direction: char) (currentPosition: string) =
         match direction with
         | 'R' ->
            printfn $"Moving right from {currentPosition}"
            myMap |> Map.find currentPosition |> snd
         | 'L' ->
            printfn $"Moving left from {currentPosition}"
            myMap |> Map.find currentPosition |> fst
         | x -> failwith $"Invalid direction: {x}"

//      let rec findPathToZZZ (directions: string) (startingPosition: string) previousStepCount =
//         match
//            directions
//            |> Seq.toList
//            |> List.fold (fun (currentPosition, stepCount) direction ->
//               navigateDirections direction currentPosition, stepCount + 1
//               ) (startingPosition, previousStepCount)
//
//         with
//         | "ZZZ", stepCount -> stepCount
//         | nowAt, stepCount ->
//            findPathToZZZ directions nowAt stepCount
//
//      findPathToZZZ directions "AAA" 0
//      |> fun x -> printfn $"Part 1: Steps to ZZZ: {x}"
//
      //// PART 2:
      let allNodesEndingOnA =
         myMap
         |> Map.filter (fun key (left, right) ->
               key |> Seq.toList |> List.last = 'A'
         )

      let checkAllKeysEndWithZ (myMap: Map<string, string * string>) =
         myMap
         |> Map.filter (fun key (left, right) ->
            key
            |> Seq.toList
            |> List.last
            |> fun x -> x <> 'Z')
         |> Map.count
         |> fun x -> x = 0

      let applyOneDirectionToMap (direction: char) (startingMap: Map<string, string * string>) =
         startingMap
         |> Map.toList
         |> List.map (fun (key,(left, right)) ->
            match direction with
            | 'R' -> right
            | 'L' -> left
            | x -> failwith $"Invalid direction: {x}"
         )
         |> List.map (fun newKey -> newKey, myMap|> Map.find newKey)
         |> Map.ofList

      let rec findPathToAllNodesEndingOnZ
         (directions: string)
         (startingMap : Map<string,string * string>)
         previousStepCount
         =
            printfn $" --> Next iteration {previousStepCount}"

            startingMap
            |> Map.toList
            |> List.map (fun x ->
               x
               |> fst
               |> Seq.toList
               |> Seq.tail
               |> Seq.head )
            |> List.toArray
            |> String
            |> (fun x -> printf $"{x}")
            if checkAllKeysEndWithZ startingMap then startingMap, previousStepCount
            else
               let nextMap, stepCounter =
                  directions
                  |> Seq.toList
                  |> List.fold (fun (currentMap: Map<string,string * string>, stepCount) direction ->
                     if checkAllKeysEndWithZ currentMap then
                        printfn $"All keys end with Z, returning step count: {stepCount}"
                        currentMap, stepCount
                     else
                        (applyOneDirectionToMap direction currentMap, (stepCount + 1))
                        |> id
                     ) (startingMap, previousStepCount)
               findPathToAllNodesEndingOnZ directions nextMap stepCounter
      printfn $"Initial list has {allNodesEndingOnA |> Map.count} nodes ending on A"

      findPathToAllNodesEndingOnZ directions allNodesEndingOnA 0
      |> fun x -> printfn $" -> Part 2 end result: {x |> snd}"

   let run =
      parseInputToAnswer input
