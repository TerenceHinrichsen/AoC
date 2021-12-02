module Day2

  let sampleInput = [
    "forward 5"
    "down 5"
    "forward 8"
    "up 3"
    "down 8"
    "forward 2" ]

  let input = TextFileReader.readFileContents "../../../Day2Input.txt"

  let run =

    let pos, depth =
      input
      |> Seq.map (fun row ->
        row.Split " "
        |> Array.pairwise
        |> Array.head)
      |> Seq.toList
      |> List.map (fun (mov, inc) ->
        match mov with
        | "forward" -> inc |> int,0
        | "down" -> 0, inc |> int
        | "up" -> 0, - (inc |> int)
        | _ -> failwith "Unexpected movement"
        )
      |> List.unzip

    let finalPos = pos |> List.sum
    let finalDepth = depth |> List.sum

    printfn $"Final position : {finalDepth * finalPos}"

    let folderFn (pos, depth, aim) (mov, inc) =
      match mov with
        | "forward" -> pos + inc, depth + (aim * inc), aim
        | "down" -> pos, depth, aim + inc
        | "up" -> pos, depth, aim - inc
        | x -> failwith "Unexpected movement"

    let pos,depth,aim =
      input
      |> Seq.map (fun row ->
        row.Split " "
        |> Array.pairwise
        |> Array.head)
      |> Seq.toList
      |> List.map (fun (mov, inc) -> mov, inc |> int)
      |> List.fold folderFn (0,0,0)


    printfn $"Final position : {pos * depth}"