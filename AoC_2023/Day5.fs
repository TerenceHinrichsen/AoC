module Day5

   open AdventOfCode
   open System
   open System.Text.RegularExpressions
   open FsToolkit.ErrorHandling

   let sampleInput = [
      "seeds: 79 14 55 13"
      ""
      "seed-to-soil map:"
      "50 98 2"
      "52 50 48"
      ""
      "soil-to-fertilizer map:"
      "0 15 37"
      "37 52 2"
      "39 0 15"
      ""
      "fertilizer-to-water map:"
      "49 53 8"
      "0 11 42"
      "42 0 7"
      "57 7 4"
      ""
      "water-to-light map:"
      "88 18 7"
      "18 25 70"
      ""
      "light-to-temperature map:"
      "45 77 23"
      "81 45 19"
      "68 64 13"
      ""
      "temperature-to-humidity map:"
      "0 69 1"
      "1 0 69"
      ""
      "humidity-to-location map:"
      "60 56 37"
      "56 93 4"

      ]
   let input =
      TextFileReader.readFileContents "../../../Day5Input.txt"

   type Range =
      { DestinationRangeStart: int
        SourceRangeStart: int
        RangeLength: int }

   let getNextPositionFromRange (input: int) (rangeL: List<Range>) =

      ()

   let parseInputToAnswer (input:List<string>) =
      // first we split on all the empty lines
      let rawInputMaps =
         input
         |> List.splitMultipleOnExcl (fun x -> x = "")

      let parseMap (map:List<string>) =
         let mapName = map.Head
         let mapLines = map.Tail
         (mapName, mapLines)

      let seedL =
         rawInputMaps
         |> List.map parseMap
         |> List.head
         |> fst
         |> fun x -> x.Split ' '
         |> Array.tail
         |> Array.toList
         |> List.map int

      let lookupMaps =
         rawInputMaps
         |> List.map parseMap
         |> List.tail// the first line are the seeds:
         |> List.filter (fun (_key, value) -> value |> List.isEmpty |> not)
         |> List.map (fun (key, value) ->
            let splitValue =
               value
               |> List.map (fun x ->
                  x.Split ' '
                  |> Array.toList
                  |> List.filter (fun x -> x <> "")
                  |> List.map int
               )
            let ranges =
               splitValue
               |> List.map (fun x ->
                  { DestinationRangeStart = x[0] |> int
                    SourceRangeStart = x[1] |> int
                    RangeLength = x[2] |> int
                  } )
            (
            key, ranges
            ) )
         |> Map.ofList

      printfn $"Mapping seeds {seedL}"
      // seed-to-soil
      seedL
      |> List.fold (fun (seedL, lookupMaps) seed ->
         let soilL =
            lookupMaps
            |> Map.find "seed-to-soil map:"
            |> List.map (fun x -> x.SourceRangeStart)
         let soil =
            soilL
            |> List.map (fun x -> x + seed)
            |> List.map (fun x -> x % 100)
            |> List.head
         printfn $"Seed {seed} -> Soil {soil}"
         (soil::seedL, lookupMaps)
         ) ([], lookupMaps)

      // soil-to-fertilizer
      // fertilizer-to-water
      // water-to-light
      // light-to-temperature
      // temperature-to-humidity
      // humidity-to-location




   let run =
      sampleInput
      |> parseInputToAnswer
