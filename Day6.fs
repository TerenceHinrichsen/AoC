module Day6

  let sampleInput = [
    "3,4,3,1,2" ]

  let input = TextFileReader.readFileContents "../../../Day6Input.txt"

  let run =
    let parsedInput =
      input
      |> Seq.head
      |> (fun x -> x.Split (","))
      |> Seq.map int
      |> Seq.toList

    let calculateNewState state =
      state |> List.map (fun (i: int) ->
        if i = 0 then [6;8] else [i - 1]) |> List.concat

    let folderFn fishL day =
      printfn $"After day {day}"
      calculateNewState fishL

//    let x =
//      [1..80]
//      |> List.fold folderFn parsedInput
//    printfn $"Part1: {x.Length}"


    // so we know the ages can only be between 0 and 8
    // so we can keep the number per age in a map and just update it?
    let startingState =
      parsedInput
      |> List.groupBy id
      |> List.map (fun (age, fishL) -> age, fishL |> List.length |> int64)
      |> Map.ofList

    let incrementAges fishAgeM day =
      printfn $"Busy with {day}"
      let numberOfNewBabies  = fishAgeM |> Map.tryFind 0 |> Option.defaultValue 0L
      let numberOfNewParents = fishAgeM |> Map.tryFind 0 |> Option.defaultValue 0L
      let numberOf1DayOld    = fishAgeM |> Map.tryFind 1 |> Option.defaultValue 0L
      let numberOf2DayOld    = fishAgeM |> Map.tryFind 2 |> Option.defaultValue 0L
      let numberOf3DayOld    = fishAgeM |> Map.tryFind 3 |> Option.defaultValue 0L
      let numberOf4DayOld    = fishAgeM |> Map.tryFind 4 |> Option.defaultValue 0L
      let numberOf5DayOld    = fishAgeM |> Map.tryFind 5 |> Option.defaultValue 0L
      let numberOf6DayOld    = fishAgeM |> Map.tryFind 6 |> Option.defaultValue 0L
      let numberOf7DayOld    = fishAgeM |> Map.tryFind 7 |> Option.defaultValue 0L
      let numberOf8DayOld    = fishAgeM |> Map.tryFind 8 |> Option.defaultValue 0L

      Map.ofList [
          (0, numberOf1DayOld)
          (1, numberOf2DayOld)
          (2, numberOf3DayOld)
          (3, numberOf4DayOld)
          (4, numberOf5DayOld)
          (5, numberOf6DayOld)
          (6, numberOf7DayOld + numberOfNewBabies)
          (7, numberOf8DayOld)
          (8, numberOfNewParents)
        ] |> id

    let x =
      [1..256]
      |> List.fold incrementAges startingState
      |> Map.toList
      |> List.sumBy snd

    printfn $"{x}"
    ()