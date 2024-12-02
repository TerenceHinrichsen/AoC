module Day1

   open AdventOfCode
   open System
   open System.Text.RegularExpressions

   let sampleInput = [
      "3   4"
      "4   3"
      "2   5"
      "1   3"
      "3   9"
      "3   3"
   ]

   let input =
      TextFileReader.readFileContents "../../../Day1Input.txt"


   let run =
      printfn "Part 1: "

      let formattedInput =

         input
         |> List.map (fun s ->
            s.Split("   ")
            |> fun x -> x[0] |> int , x[1] |> int
            )
         |> List.unzip
         |> fun (l1, l2) -> l1 |> List.sort, l2 |> List.sort
         |> fun (l1, l2) ->
            l1 |> List.mapi (fun i x -> x - l2[i] |> abs)
            |> List.sum
         |> fun x -> printfn $"Solution is: {x}"
      ()

      printfn "Part 2:  ----------------------------------------------------- "

      input
      |> List.map (fun s ->
         s.Split("   ")
         |> fun x -> x[0] |> int , x[1] |> int
         )
      |> List.unzip
      |> fun (l1, l2) -> l1 |> List.sort, l2 |> List.sort
      |> fun (l1, l2) ->
         l1
         |> List.map (fun x ->
            let numOccurInB = l2 |> List.filter (fun y -> y = x) |> List.length
            x * numOccurInB
            )
         |> List.sum
      |> fun x -> printfn $"Solution is: {x}"
