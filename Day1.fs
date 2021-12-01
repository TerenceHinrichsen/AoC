module Day1

  let sampleInput = [
    199
    200
    208
    210
    200
    207
    240
    269
    260
    263    ]

  let input = TextFileReader.readFileContents "../../../Day1Input.txt"
  let run =

    input
    |> Seq.toList
    |> List.pairwise
    |> List.sumBy (fun (x, y) ->
      if ( y|> int) > (x |> int) then 1 else 0 )
    |> printfn "Answer for part 1 :%i"

    input
    |> Seq.toList
    |> List.map int
    |> List.windowed 3
    |> List.map (List.sum)
    |> List.pairwise
    |> List.sumBy (fun (x, y) -> if ( y|> int) > (x |> int) then 1 else 0 )
    |> printfn "Answer for part 2 :%i"
