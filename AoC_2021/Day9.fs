module Day9

  let sampleInput = [
      "2199943210"
      "3987894921"
      "9856789892"
      "8767896789"
      "9899965678"
    ]

  let input = TextFileReader.readFileContents "../../../Day9Input.txt" |> Seq.toList

  let run =
    let inline charToInt c = int c - int '0'

    let puzzleInput =
      input
      |> List.map (fun x -> x |> Seq.toList)

    let areaMap =
      puzzleInput
      |> List.mapi (fun rowI row -> row |> List.mapi (fun colI y -> (rowI, colI), y))
      |> List.concat
      |> Map.ofList

    let lowestEdgeI = puzzleInput |> List.length

    let furthestEdgeI = puzzleInput |> List.map (fun row -> row |> List.length) |> List.max

    let lowPoints =
      areaMap
      |> Map.map (fun (rowI, colI) char ->
        let upValue =
          if rowI = 0 then None
          else areaMap |> Map.tryFind (rowI - 1, colI)
          |> Option.defaultValue '9'
        let downValue =
          if rowI = lowestEdgeI then None
          else areaMap |> Map.tryFind (rowI + 1, colI)
          |> Option.defaultValue '9'
        let leftValue =
          if colI = 0 then None
          else areaMap |> Map.tryFind (rowI, colI - 1)
          |> Option.defaultValue '9'
        let rightValue =
          if colI = furthestEdgeI then None
          else areaMap |> Map.tryFind (rowI, colI + 1)
          |> Option.defaultValue '9'

        let isLowest =
          char < upValue && char < leftValue && char < rightValue && char < downValue
        let riskLevel = charToInt char |> (+) 1
        printfn $"Looking at char {char}"
        printfn $"Up is {upValue}"
        printfn $"Down is {downValue}"
        printfn $"Left is {leftValue}"
        printfn $"Right is {rightValue}"
        printfn $"Is this value lower? {isLowest}"

        printfn $"Risk level {riskLevel}"

        if isLowest then riskLevel else 0 )

    lowPoints
    |> Map.toList
    |> List.sumBy (fun (key, value) -> value)
    |> printfn "%A"
    ()
