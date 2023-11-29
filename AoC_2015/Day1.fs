module Day1

  let sampleInput = "))((((("

  let input = TextFileReader.readFileContents "../../../Day1Input.txt"
  let run =

     //part 1:
     input
     |> Seq.toList
     |> List.head
     |> Seq.toList
     |> List.groupBy id
     |> List.map (fun (k, v) -> (k, List.length v))
     |> List.map (fun (c, count) -> if c = '(' then count else -count)
     |> List.sum
     |> printfn "Part 1: %d"

       //part 2:
     input
     |> Seq.toList
     |> List.head
     |> Seq.toList
     |> List.scan (fun acc c -> if c = '(' then acc + 1 else acc - 1) 0
     |> List.findIndex (fun x -> x = -1)
     |> printfn "Part 2: %d"
