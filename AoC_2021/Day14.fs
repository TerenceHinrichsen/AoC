module Day14

  let sampleInput = [
    "NNCB"
    ""
    "CH -> B"
    "HH -> N"
    "CB -> H"
    "NH -> C"
    "HB -> C"
    "HC -> B"
    "HN -> C"
    "NN -> C"
    "BH -> H"
    "NC -> B"
    "NB -> B"
    "BN -> B"
    "BB -> N"
    "BC -> B"
    "CC -> N"
    "CN -> C"
  ]

  let input = TextFileReader.readFileContents "../../../Day14Input.txt" |> List.ofSeq
  let inline charToInt c = int c - int '0'

  let run =

    let puzzleInput = input

    let template = puzzleInput |> List.head

    let insertionRules =
      puzzleInput
      |> List.skip 2
      |> List.map (fun rule -> rule.Split(" -> ") |> List.ofSeq)
      |> List.map (fun rule -> rule.[0], rule.[1])
      |> Map.ofList

    let splitStringToPairs inputString =
      inputString
      |> List.ofSeq
      |> List.pairwise
      |> List.map (fun (x, y) -> $"{x}{y}")

    let singleStep inputString run =
//      printfn $"Before run {run} the string is {inputString}"
      inputString
      |> splitStringToPairs
      |> List.map (fun x ->
        let charToInsert =
          insertionRules
          |> Map.tryFind x
          |> Option.defaultValue ""
        $"{x.[0]}{charToInsert}" )
      |> List.fold (fun acc x -> $"{acc}{x}") ""
      |> fun x -> $"{x}{inputString |> Seq.rev |> Seq.head}"

    let finalString =
      [1..10]
      |> List.fold singleStep template

    printfn $"After all, the length is : {finalString.Length}"
    let charCounts =
      finalString
      |> Seq.toList
      |> List.groupBy (fun x -> x)
      |> List.map (fun (char, charL) -> char, charL |> List.length)

    let mostCommon =
      charCounts
      |> List.maxBy snd

    let leastCommon =
      charCounts
      |> List.minBy snd

    printfn $"Most common {mostCommon} vs least common {leastCommon}"

    printfn $"Part 1 answer is : {(mostCommon |> snd) - (leastCommon |> snd)}"

    let insertionRules =
      puzzleInput
      |> List.skip 2
      |> List.map (fun rule -> rule.Split(" -> ") |> List.ofSeq)
      |> List.map (fun rule -> rule.[0], rule.[1].[0])
      |> Map.ofList

    let partTwoStep (inputArray: List<char>) run  =
      let pairsToLookup =
        inputArray
        |> List.pairwise
        |> List.map (fun (x, y) -> $"{x}{y}")

      pairsToLookup
      |> List.map (fun x ->
        let charToInsert =
          insertionRules
          |> Map.tryFind x
        match charToInsert with
        | Some c -> $"{x.[0]}{c}" |> List.ofSeq
        | None -> x |> List.ofSeq)
      |> List.concat
      |> List.append [inputArray |> List.rev |> List.head]

    let part2String =
      [1..40]
      |> List.fold partTwoStep (template |> List.ofSeq)

    printfn $"Result {part2String.Length}"

    ()