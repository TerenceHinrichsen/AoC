module Day13

  let sampleInput = [
    "6,10"
    "0,14"
    "9,10"
    "0,3"
    "10,4"
    "4,11"
    "6,0"
    "6,12"
    "4,1"
    "0,13"
    "10,12"
    "3,4"
    "3,0"
    "8,4"
    "1,10"
    "2,14"
    "8,10"
    "9,0"
    ""
    "fold along y=7"
    "fold along x=5"
  ]

  let input = TextFileReader.readFileContents "../../../Day13Input.txt" |> List.ofSeq
  let inline charToInt c = int c - int '0'

  let run =

    let puzzleInput =
      let indexOfInstructions =
        sampleInput
        |> List.findIndex (fun x -> x = "")

      sampleInput
      |> List.splitAt (indexOfInstructions + 1)

    let listOfDots =
      puzzleInput
      |> fst
      |> List.takeWhile (fun x -> x <> "")
      |> List.map (fun s -> s.Split (",") |> Seq.toList)
      |> List.map (fun sl -> sl.[0] |> int, sl.[1] |> int)

    let instructions = puzzleInput |> snd

    let maxColumn = listOfDots |> List.maxBy fst |> fst
    let maxRow = listOfDots |> List.maxBy snd |> snd

    let markedGrid =

      [0..maxRow]
      |> List.map ( fun row ->
          [0..maxColumn]
          |> List.map (fun col ->
            if
                listOfDots
                |> List.tryFind (fun (lCol, lRow) -> lCol = col &&  lRow = row)
                |> Option.isNone
            then (col, row), false
            else (col, row), true
            )
        )
      |> List.concat

    let firstInstruction =
      instructions
      |> List.head
      |> fun x -> x.Substring(11, x.Length - 11)

    printfn $"First instruction : {firstInstruction}"
    let firstInstructionIndex = firstInstruction.Split "=" |> Seq.toList |> List.tail |> List.head |> int

    let x =
     match firstInstruction.Substring(0,1) with
     | "x" ->
       printfn $"Fold along x at position {firstInstructionIndex}."
       // vertical fold
       markedGrid
       |> List.map (fun ((col, row), isOn) ->
         let firstPart =
           markedGrid |> List.filter (fun ((col, row), _checked ) -> row < firstInstructionIndex)
         let secondPart =
           markedGrid |> List.filter (fun ((col, row), _checked ) -> row > firstInstructionIndex)
         firstPart, secondPart
         )

     | "y" ->
       printfn $"Fold along y at position {firstInstructionIndex}."
       // horizontal fold
       markedGrid
       |> List.map (fun ((col, row), isOn) ->
         let firstPart =
           markedGrid |> List.filter (fun ((col, row), _checked ) -> row < firstInstructionIndex)
         let secondPart =
           markedGrid |> List.filter (fun ((col, row), _checked ) -> row > firstInstructionIndex)
         firstPart, secondPart
         )

     | x -> failwith $"Format of instruction is invalid. Not expecting {x}"



    ()