module Day1

   let sampleInput = [
      "1000"
      "2000"
      "3000"

      "4000"

      "5000"
      "6000"

      "7000"
      "8000"
      "9000"

      "1000"
   ]



   let splitBy v list =
     let yieldRevNonEmpty list =
       if list = [] then []
       else [List.rev list]

     let rec loop groupSoFar list = seq {
       match list with
       | [] -> yield! yieldRevNonEmpty groupSoFar
       | head::tail when head = v ->
           yield! yieldRevNonEmpty groupSoFar
           yield! loop [] tail
       | head::tail ->
           yield! loop (head::groupSoFar) tail }
     loop [] list |> List.ofSeq

   let input = TextFileReader.readFileContents "../../../Day1Input.txt"
   let run =

      let calorieCountByElf =
         input
         |> Seq.toList
         |> List.map (fun x -> if x = "" then "BREAK" else x)
         |> splitBy "BREAK"
         |> List.map (fun sL ->
            sL
            |> List.map (fun s -> int s)
            |> List.sum
         )

      calorieCountByElf
      |> List.sortDescending
      |> List.head
      |> fun x -> printfn $"PART1: Top calorie count by elf: {x}"

      calorieCountByElf
      |> List.sortDescending
      |> List.take 3
      |> List.sum
      |> fun x -> printfn $"PART 2: Top three sum: {x}"