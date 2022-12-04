namespace AdventOfCode
open System

module Day4 =

   let sampleInput = [
      "2-4,6-8"
      "2-3,4-5"
      "5-7,7-9"
      "2-8,3-7"
      "6-6,4-6"
      "2-6,4-8"
   ]

   let input = TextFileReader.readFileContents "../../../Day4Input.txt"
   module Part1 =
      let run =

         let parseInputToStartAndEnd (row: string) =
            row.Split ","
            |> fun y ->
               y.[0].Split "-"
               |> fun z -> z[0] |> int64, z[1] |> int64
               , y.[1].Split "-"
               |> fun z -> z[0] |> int64 , z[1] |> int64

         input
         |> Seq.toList
         |> List.map parseInputToStartAndEnd
         |> List.map (fun ((p1Start, p1End), (p2Start, p2End)) ->
            if p1Start >= p2Start && p1End <= p2End then
               1
            elif
               p2Start >= p1Start && p2End <= p1End then
               1
            else 0
            )
         |> List.sum

         |> fun x -> printfn $"Part 1: {x}"

   module Part2 =
      let run =

         let parseInputToStartAndEnd (row: string) =
            row.Split ","
            |> fun y ->
               y.[0].Split "-"
               |> fun z -> z[0] |> int64, z[1] |> int64
               , y.[1].Split "-"
               |> fun z -> z[0] |> int64 , z[1] |> int64


         input
         |> Seq.toList
         |> List.map parseInputToStartAndEnd
         |> List.map (fun ((p1Start, p1End), (p2Start, p2End)) ->
            if not ( p2Start > p1End || p2End < p1Start) then
               1
            else 0
            )
         |> List.sum

         |> fun x -> printfn $"Part 2: {x}"

   let run =
      Part1.run
      Part2.run