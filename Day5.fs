module Day5

  open System.Net

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
    "5,5 -> 8,2"
  ]

  let input = TextFileReader.readFileContents "../../../Day5Input.txt" |> Seq.toList

  let pr s l = l |> List.iter (fun x -> printfn $"{s} - {x}"); l
  let run =

    let puzzleInput =
      input
      |> List.map (fun row -> row.Split " -> ")
      |> List.map (fun lineP ->
        let startPosition =
          lineP.[0] |> fun p -> p.Split "," |> Seq.take 2 |> Seq.pairwise |> Seq.toList |> List.map (fun s -> s |> fst |> int, s |> snd |> int) |> List.head
        let endPosition =
          lineP.[1] |> fun p -> p.Split "," |> Seq.take 2 |> Seq.pairwise |> Seq.toList |> List.map (fun s -> s |> fst |> int, s |> snd |> int) |> List.head
//        printfn $"Start position : {startPosition} to End position : {endPosition}"
        startPosition, endPosition
        )

    let part1 =
      let allIntersections =
        puzzleInput
        |> List.map ( fun ((startX, startY), (endX, endY) ) ->
          printfn $"Calculating line ({startX},{startY}) -> ({endX},{endY})"

          // horizontal line
          if startX = endX  then
            printfn "Horizontal line"
            if startY > endY then
              [endY..startY]
              |> List.map (fun y -> [startX, y])
                else
              [startY..endY]
              |> List.map (fun y -> [startX, y])
          elif startY = endY then
            printfn "Vertical line"
            // vertical line
            if startX > endX then
              [endX..startX]
              |> List.map (fun x -> [x, startY])
              else
              [startX..endX]
              |> List.map (fun x -> [x, startY])
          else
            printfn "Diagonal line"
            []
          |> pr "Positions"
          )
        |> List.concat
        |> List.concat
        |> List.sort

      allIntersections
      |> List.groupBy id
      |> List.map (fun (position, lines) -> position, lines.Length )

    part1
    |> List.filter (fun (position, length) -> length >= 2)
    |> List.length
    |> printfn "Number of positions with two lines : %A"

    let part2 =
      let allIntersections =
        puzzleInput
        |> List.map ( fun ((startX, startY), (endX, endY) ) ->
          printfn $"Calculating line ({startX},{startY}) -> ({endX},{endY})"

          // horizontal line
          if startX = endX  then
            printfn "Horizontal line"
            if startY > endY then
              [endY..startY]
              |> List.map (fun y -> [startX, y])
                else
              [startY..endY]
              |> List.map (fun y -> [startX, y])
          elif startY = endY then
            printfn "Vertical line"
            // vertical line
            if startX > endX then
              [endX..startX]
              |> List.map (fun x -> [x, startY])
              else
              [startX..endX]
              |> List.map (fun x -> [x, startY])
          else
            printfn "Diagonal line"
            match endX > startX, endY > startY with
            | true, true
            | true, false
            | false, true
            | false, false ->
            []
          |> pr "Positions"
          )
        |> List.concat
        |> List.concat
        |> List.sort

      allIntersections
      |> List.groupBy id
      |> List.map (fun (position, lines) -> position, lines.Length )

    part2
    |> List.filter (fun (position, length) -> length >= 2)
    |> List.length
    |> printfn "Number of positions with two lines : %A"


    ()