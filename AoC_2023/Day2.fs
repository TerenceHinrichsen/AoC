module Day2

   open AdventOfCode
   open System
   open System.Text.RegularExpressions

   let sampleInput = [
      "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
      "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
      "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
      "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
      "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
   ]

   let input =
      TextFileReader.readFileContents "../../../Day2Input.txt"

   let maxRed = 12
   let maxGreen = 13
   let maxBlue = 14

   type PickedCube = { Colour: string; Count: int; Round : int}
   type GameResult =
      {
         GameId: int
         PickedCubes : List<PickedCube>
      }

   let parseGameResult (input: string) =

      let gameId =
         input.Split(":")[0]
         |> Seq.filter (fun c -> Char.IsNumber c)
         |> Seq.toArray
         |> String
         |> int

      let pickedCubes =
         input.Split(":")[1] // all rounds as one string
         |> fun x -> x.Split(";") // split into rounds
         |> Seq.map (fun x ->
//            printfn $"Next round: {x}"
            x.Trim()) // trim whitespace
         |> Seq.filter (fun x -> x <> "") // remove empty strings
         |> Seq.map (fun x -> x.Split(",")) // split into cubes
         |> Seq.mapi (fun round (x: string array) ->
            //first is the count and then the colour
//            printfn $"Round {round + 1} has {x.Length} colour cubes"
            x
            |> Seq.map (fun x ->
               let y = x.Trim().Split(" ")
//               printfn $"Colour: {y.[1]} Count: {y.[0]}"
               {
                     Round = round + 1
                     Colour = y.[1]
                     Count = y.[0] |> fun x -> x.Trim() |> int
               }
            )
            |> Seq.toList
            )

         |> List.ofSeq
         |> List.concat

      { GameId = gameId; PickedCubes = pickedCubes }

   let isGamePossible (gameResult : GameResult) =
      if
         gameResult.PickedCubes
         |> List.filter (fun x ->
            match x.Colour with
            | "red" -> x.Count > maxRed
            | "green" -> x.Count > maxGreen
            | "blue" -> x.Count > maxBlue
            | x -> failwith $"WTF {x}"
         )
         |> List.length
         > 0
         then false
      else true

   let getMinPerColour (gameResults : List<GameResult>) =

      gameResults
      |> List.map (fun x ->
         let minPerColour =
            x.PickedCubes
            |> List.groupBy (fun x -> x.Colour)
            |> List.map (fun (colour, cubes) ->
               cubes |> List.maxBy (fun x -> x.Count)
            )
         x.GameId, minPerColour)
      |> List.map (fun (gameId, results) ->
         gameId, results[0].Count * results[1].Count * results[2].Count
         )
      |> List.map (fun (gameId, result) ->
         printfn $"Game {gameId} has a result of {result}"
         result)


   let parseInputToAnswer (input: List<string>) =


      input
      |> List.map parseGameResult
      |> List.filter isGamePossible
      |> List.sumBy (fun x -> x.GameId)
      |> fun x -> printfn $"Part 1: {x}"

      input
      |> List.map parseGameResult
      |> getMinPerColour
      |> List.sum
      |> fun x -> printfn $"Part 2: {x}"

   let run =
      input
      |> parseInputToAnswer
