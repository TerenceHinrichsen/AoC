module Day3

   open AdventOfCode
   open System
   open System.Text.RegularExpressions

   let sampleInput = [
      "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
   ]

   let sampleInputP2 = [
      "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
   ]

   let input =
      TextFileReader.readFileContents "../../../Day3Input.txt"

   let regex = Regex(@"mul\((\d+),(\d+)\)")

   let run =

      input
      |> List.map (fun s ->
         let matches = regex.Matches s
         matches
         |> Seq.map (fun x -> x.Groups[1].Value, x.Groups.[2].Value)
         |> Seq.toList
         |> List.map (fun (a, b) -> int a * int b)
         |> List.sum
      )
      |> List.sum
      |> fun x -> printfn $"Part 1: {x}"

      let doRegex = @"do\(\)"
      let dontRegex = @"don't\(\)"
      let multiplyRegex = @"mul\((\d+),(\d+)\)"

      let getMatchL =
         input
         |> List.map (fun s ->
            Regex.Matches(s, $"{regex}|{doRegex}|{dontRegex}")
            |> Seq.toList
         )
         |> List.concat

      let foldFn (isEnabled, acc) (singleMatch: Match) =
         match singleMatch.Value with
         | s when Regex.IsMatch(s, doRegex) -> (true, acc)
         | s when Regex.IsMatch(s, dontRegex) -> (false, acc)
         | s when Regex.IsMatch(s, multiplyRegex) ->
            if isEnabled then
               Regex.Match(s, multiplyRegex).Groups
               |> fun x -> x[1].Value |> int, x[2].Value |> int
               |> fun (a, b) -> (isEnabled, acc + a * b)
            else
               (isEnabled, acc)
         | _ -> (isEnabled, acc)

      let result =
         getMatchL
         |> List.fold foldFn (true, 0)
      printfn $"Part 2: {result}"
