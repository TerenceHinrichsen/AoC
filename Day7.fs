module Day7

  let sampleInput = [
    "16,1,2,0,4,2,7,1,2,14" ]

  let input = TextFileReader.readFileContents "../../../Day7Input.txt" |> Seq.toList

  let run =

    let puzzleInput =
      input
      |> List.head
      |> (fun x -> x.Split(","))
      |> Seq.map int
      |> Seq.toList

    let uniquePositions = puzzleInput |> List.distinct

    let fuelRequiredToMoveToEachPosition =
      puzzleInput
        |> List.map (fun crabPos ->
          [uniquePositions |> List.min .. uniquePositions |> List.max]
          |> List.map (fun hpos ->
            let moveCost =
              let numberOfStepsToMove = abs(hpos - crabPos)
              [1..numberOfStepsToMove] |> List.sum
            crabPos, hpos, moveCost ))

    let fuelPerPosition =
      fuelRequiredToMoveToEachPosition
      |> List.concat
      |> List.groupBy (fun (crabPos, pos, fuelCost) -> pos )
      |> List.map (fun (pos, crabL) -> pos, crabL |> List.sumBy (fun (_, _, fuelCost) -> fuelCost))
      |> List.map (fun (pos, totalFuelCost) ->
        printfn $"For all to move to position {pos} the cost would be {totalFuelCost}"
        pos, totalFuelCost)
      |> List.minBy (fun (pos, totalFuelCost) -> totalFuelCost)
      |> fun x -> printfn $"The cheapest move would be to position {x |> fst} at a cost of {x |> snd}"; x
    |> id
    ()