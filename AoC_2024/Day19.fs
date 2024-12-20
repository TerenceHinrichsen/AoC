module Day19

   open AdventOfCode
   open System
   open System.Text.RegularExpressions
   open System.Threading.Tasks

   let sampleInput = seq [
      "r, wr, b, g, bwu, rb, gb, br"
      ""
      "brwrr"
      "bggr"
      "gbbr"
      "rrbgbr"
      "ubwu"
      "bwurrg"
      "brgr"
      "bbrgwb"
   ]

   let input =
      TextFileReader.readFileContents "../../../Day19Input.txt"

   let run =
      let patterns =
         input[0].Split(", ")

      let designs =
         input
         |> List.skip 2

      let rec possibleOptions design acc =
         if design = "" then
            1L, acc
         else
            match Map.tryFind design acc with
            | Some result -> result, acc
            | None ->
               let result =
                  patterns
                  |> Seq.filter (fun p -> design.StartsWith(p))
                  |> Seq.sumBy (fun p -> possibleOptions (design.Substring(p.Length)) acc  |> fst)
               let newAcc =
                  acc
                  |> Map.add design result
               result, newAcc

      let possibleCount =
         designs
         |> List.filter (fun d -> possibleOptions d Map.empty |> fst > 0)
         |> List.length

      printfn $"Possible: {possibleCount}"

      let possibleCountSum =
         designs
         |> List.sumBy (fun x ->  (possibleOptions x Map.empty) |> fst)

      printfn $"Possible options: {possibleCountSum}"


      ()
