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

      let mutable cache : Map<string, int64> = Map.empty

      let rec possibleOptions design =
         if design = "" then
            1L
         else
            match Map.tryFind design cache with
            | Some result -> result
            | None ->
               let result =
                  patterns
                  |> Seq.filter (fun p -> design.StartsWith(p))
                  |> Seq.sumBy (fun p -> possibleOptions (design.Substring(p.Length)))
               cache <- Map.add design result cache
               result

      let possibleCount =
         designs
         |> List.filter (fun d -> possibleOptions d > 0)
         |> List.length

      printfn $"Possible: {possibleCount}"

      let possibleCountSum =
         designs
         |> List.sumBy possibleOptions

      printfn $"Possible options: {possibleCountSum}"


      ()
