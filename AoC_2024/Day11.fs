module Day11

   open AdventOfCode
   open System
   open System.Text.RegularExpressions
   open System.Threading.Tasks

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

      let applyRulesV2 (number: int64) =
         match number with
         | 0L -> [1L]
         | _ ->
            let digits = number.ToString().ToCharArray()
            if digits.Length % 2 = 0 then
               let half = digits.Length / 2
               let first = String(digits[0..half-1]) |> int64
               let second = String(digits[half..]) |> int64
               [first; second]
            else
               [number * 2024L]

//      let rec iterateSingleStone (stoneL: List<int>) iterations =
//         if iterations = 0 then
//            stoneL
//         else
//            let newStoneL =
//               List.map (fun x -> applyRulesV2 x)
//               iterateSingleStone (newStone |> List.head) (iterations - 1)
//            ()
//
//      let attempt10 =
//         digitList
//         |> List.toArray
//         |> Array.Parallel.map (fun stone -> iterateSingleStone stone 75)
//
//      printfn $"Part 2: After 75 blinks, there are {attempt10 |> Array.sum} stones"

      ()
