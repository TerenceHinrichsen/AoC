module Day25

   open AdventOfCode
   open System
   open System.Text.RegularExpressions
   open System.Threading.Tasks

   let sampleInput = [
      "#####"
      ".####"
      ".####"
      ".####"
      ".#.#."
      ".#..."
      "....."
      ""
      "#####"
      "##.##"
      ".#.##"
      "...##"
      "...#."
      "...#."
      "....."
      ""
      "....."
      "#...."
      "#...."
      "#...#"
      "#.#.#"
      "#.###"
      "#####"
      ""
      "....."
      "....."
      "#.#.."
      "###.."
      "###.#"
      "###.#"
      "#####"
      ""
      "....."
      "....."
      "....."
      "#...."
      "#.#.."
      "#.#.#"
      "#####"
   ]

   let input =
      TextFileReader.readFileContents "../../../Day25Input.txt"


   type Input = | Lock | Key

   let isLockOrKey string = if string = "#####" then Lock else Key

   let convertInputToIntegerList (inputL: List<string>) =
      inputL
      |> List.map (fun s ->
         s |> Seq.map (fun c -> if c = '#' then 1 else 0)
         |> Seq.toList : List<int>
      )
      |> List.transpose
      |> List.map (fun l -> l |> List.fold (fun acc i -> acc + i) -1)

   let checkFit (key: List<int>) (lock: List<int>) =
      key
      |> List.zip lock
      |> List.forall (fun (k, l) ->
         let result = k + l <= 5
         result)

   let run =
      let mappedInput =
         input
         |> List.splitMultipleOnExcl (fun s -> s = "")
         |> List.map (fun l -> l.Head |> isLockOrKey, l |> convertInputToIntegerList )

      let locks = mappedInput |> List.filter (fun (i, _) -> i = Lock)
      let keys = mappedInput |> List.filter (fun (i, _) -> i = Key)

      let lockKeyFits =
         keys
         |> List.map (fun (keyType, key) ->
            let fittingLocks =
               locks
               |> List.filter (fun (_, lock) -> checkFit key lock)

            (keyType, key, fittingLocks.Length) // Include the count of fitting locks
         )
         |> List.filter (fun (_, _, fitCount) -> fitCount > 0) // Keep only keys that fit at least one lock
         |> List.sumBy (fun (_, _, fitCount) -> fitCount) // Sum the count of fitting locks

      lockKeyFits

   printfn $"Part 1 output: {run}"
