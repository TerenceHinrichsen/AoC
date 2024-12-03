module Day2

   open AdventOfCode
   open System
   open System.Text.RegularExpressions

   let sampleInput = [
      "7 6 4 2 1"
      "1 2 7 8 9"
      "9 7 6 2 1"
      "1 3 2 4 5"
      "8 6 4 4 1"
      "1 3 6 7 9"
   ]

   let input =
      TextFileReader.readFileContents "../../../Day2Input.txt"

   let remove_at_index (list: List<int>) (index: int)
      =
      List.concat [
         list[0..index - 1]
         list[index + 1..]
      ]

   let run =

      let checkIfSafe (input: List<int>) : bool =
         let isDecreasing =
            input
            |> List.pairwise
            |> List.head
            |> fun (a, b) -> a > b
         let isSaveL =
            if isDecreasing then
               input
               |> List.pairwise
               |> List.map (fun (a, b) ->
                  if a <= b then false
                  else
                     not (abs( a - b) > 3) )
            else
               input
               |> List.pairwise
               |> List.map (fun (a, b) ->
                  if a >= b then false
                  else
                     not (abs( b - a) > 3) )

         isSaveL |> List.tryFind (fun x -> x = false) |> Option.isNone


      let solvePart1 (input: List<string>) =
         input
         |> List.map ( fun x -> x.Split " " |> Array.map int |> List.ofArray)
         |> List.map checkIfSafe
         |> List.filter (fun x -> x = true)
         |> List.length
         |> fun x -> printfn $"Result: {x}"

      solvePart1 input

      let checkIfSafeP2 (input: List<int>) =
         if checkIfSafe input
         then true // don't have to do anything, is already safe.
         else
            // list must be either increasing or decreasing and each step must be at least 1 and at most 3.
            // find first invalid row
            [ 0..input.Length - 1]
            |> List.exists ( fun index ->
               remove_at_index input index |> checkIfSafe
               )

      let solvePart2 (input: List<string>) =
         input
         |> List.map ( fun x -> x.Split " " |> Array.map int |> List.ofArray)
         |> List.map checkIfSafeP2
         |> List.filter (fun x -> x = true)

         |> List.length
         |> fun x -> printfn $"Result: {x}"

      solvePart2 input
