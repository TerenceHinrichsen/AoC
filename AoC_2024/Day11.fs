module Day11

   open AdventOfCode
   open System
   open System.Text.RegularExpressions

   let sampleInput = seq [
      "125 17"
   ]

   let input =
      TextFileReader.readFileContents "../../../Day11Input.txt"

   //rules:
   // 1. 0 -> 1
   // 2. Even digits -> split into two
   // 3. Old number * 2024

   let run =

      let applyRules (number : int64) =
         match number with
         | 0L ->
            [1L]
         | _  ->
            let digits = number.ToString().ToCharArray()
            if digits.Length % 2 = 0 then
               let half = digits.Length / 2
               let firstHalf = String(digits) |> int64 |> fun x -> x.ToString().Substring(0, half).ToCharArray()
               let secondHalf = String(digits) |> int64 |> fun x -> x.ToString().Substring(half).ToCharArray()
               let first = String(firstHalf) |> int64
               let second = String(secondHalf) |> int64
               [first; second]
            else
               [number * 2024L]

      let digitList =
         input
         |> Seq.head
         |> fun x -> x.Split ' '
         |> Array.toList
         |> List.map int64

      let part1 =
         [0..24]
         |> List.fold (fun acc _iteration ->
            let newStoneList =
               acc
               |> List.map applyRules
               |> List.concat
            newStoneList) digitList

      printfn $"After 25 blinks, there are {part1.Length} stones"

      printfn "Part 2, fold does not work, lets try recursion."

      let applyRulesV2 (number : int64) =
         match number with
         | 0L ->
            seq [1L]
         | _  ->
            let digits = number.ToString().ToCharArray()
            if digits.Length % 2 = 0 then
               let half = digits.Length / 2
               let first = String(digits[0..half-1]) |> int64
               let second = String(digits[half..]) |> int64
               seq [first; second]
            else
               seq [number * 2024L]

      let rec blink iterations currentList =
         if iterations = 0 then
            currentList
         else
            let newList =
               currentList
               |> Seq.collect applyRulesV2
            blink (iterations - 1) newList

      let part2 = blink 75 (digitList |> List.toSeq)

      part2 |> Seq.toList |> List.length
      |> fun x -> printfn $"After 75 blinks, there are {x} stones"

      ()
