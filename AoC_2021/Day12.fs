module Day12

  let sampleInput = [
    "start-A"
    "start-b"
    "A-c"
    "A-b"
    "b-d"
    "A-end"
    "b-end"
  ]

  let input = TextFileReader.readFileContents "../../../Day12Input.txt" |> List.ofSeq
  let inline charToInt c = int c - int '0'

  let run =

    let puzzleInput =
      sampleInput
      |> List.map (fun s ->
          let x = s.Split("-")
          x.[0], x.[1])

    let uniquePaths =
      puzzleInput
      |> List.map (fun (startP, endP) ->
        // get a list of all the unique positions by returning both start -> end and end -> start
        [startP, endP; endP, startP]  )
      |> List.concat
      |> List.distinct

    uniquePaths
    |> id
    |> ignore
    ()