module Day5

  let sampleInput = [
      "0,9 -> 5,9"
      "8,0 -> 0,8"
      "9,4 -> 3,4"
      "2,2 -> 2,1"
      "7,0 -> 7,4"
      "6,4 -> 2,0"
      "0,9 -> 2,9"
      "3,4 -> 1,4"
      "0,0 -> 8,8"
      "5,5 -> 8,2"  ]

  let input = TextFileReader.readFileContents "../../../Day3Input.txt"

  let run =
    let numberSequence =
      sampleInput
      |> List.head
      |> fun l -> l.Split(",")
      |> Seq.toList
      |> List.map int

    let boards =
      sampleInput
      |> List.skip 1
      |> List.chunkBySize 6
      |> List.map (List.skip 1)
      |> List.map (List.map (fun x ->
        x.Split(" ")
        |> Seq.filter (fun x -> x <> "")
        |> Seq.toList |> List.map int
        ))


    printfn $"The first sequence of numbers are : {numberSequence |> List.take 5}"

    printfn $"{boards.[0]}"
    printfn $"{boards.[1]}"
    printfn $"{boards.[2]}"


    ()