module Day11

  let sampleInput = [
      "5483143223"
      "2745854711"
      "5264556173"
      "6141336146"
      "6357385478"
      "4167524645"
      "2176841721"
      "6882881134"
      "4846848554"
      "5283751526"
    ]

  let input = TextFileReader.readFileContents "../../../Day11Input.txt" |> List.ofSeq
  let inline charToInt c = int c - int '0'

  type Row = int * int * int * bool
  type Grid = List<Row>

  let printGrid (grid: Grid) =
    grid
    |> List.iteri (fun index row ->
      let (a, b, c, d) = row
      printf $"{c}"
      if (index % 9) <> a
      then ()
      else
        if b = 0 then () else printfn "" )
    printfn ""

  let lightUp (row: Row) : Row =
    let ri, cI, v, o = row
    if o = false && v > 9 then ri, cI, 0, true else ri, cI, v, o

  let get10sFromGrid (grid: Grid) =
    grid
    |> List.filter (fun row ->
      let _, _, v, _ = row
      v > 9)

  let resetAllLit (grid: Grid) =
    printfn "Reset lit to false"
    grid
    |> List.map (fun row ->
      let ri, cI, v, _o = row
      ri, cI, (if v > 9 then 0 else v), false)

  let rec reactToNeighbour (grid: Grid) : Grid =
    match get10sFromGrid grid with
    | [] ->
      printfn "Grid does not contain any 9s"
      grid |> printGrid
      grid
    | listOf9s ->
      printfn $"Grid contains {listOf9s.Length} 9s, need to trigger reaction"
      printGrid grid
      let mapOf9s =
        listOf9s
        |> List.map (fun row ->
           let ri, ci, v, lit = row
           (ri,ci), v)
        |> Map.ofList

      grid
      |> List.map lightUp
      |> List.map (fun row ->
           let ri, ci, v, lit = row
           let upO        = Map.tryFind (ri - 1, ci    ) mapOf9s
           let downO      = Map.tryFind (ri + 1, ci    ) mapOf9s
           let leftO      = Map.tryFind (ri    , ci - 1) mapOf9s
           let rightO     = Map.tryFind (ri    , ci + 1) mapOf9s
           let diagUpO    = Map.tryFind (ri - 1, ci - 1) mapOf9s
           let diagDownO  = Map.tryFind (ri + 1, ci + 1) mapOf9s
           let diagLeftO  = Map.tryFind (ri - 1, ci + 1) mapOf9s
           let diagRightO = Map.tryFind (ri + 1, ci - 1) mapOf9s

           let newValue =
             if upO             |> Option.isSome && not lit then v + 1
             else if downO      |> Option.isSome && not lit then v + 1
             else if leftO      |> Option.isSome && not lit then v + 1
             else if rightO     |> Option.isSome && not lit then v + 1
             else if diagUpO    |> Option.isSome && not lit then v + 1
             else if diagDownO  |> Option.isSome && not lit then v + 1
             else if diagLeftO  |> Option.isSome && not lit then v + 1
             else if diagRightO |> Option.isSome && not lit then v + 1
             else v
           ri, ci, newValue, lit
        )
      |> reactToNeighbour

  let addEnergyToAll (grid: Grid) =
    grid
    |> List.map (fun row ->
      let rI, cI, v, o = row
      rI, cI, v + 1, o  ) : Grid

  let runThrough grid run =
    printfn $"Iteration {run}"
    grid
    |> addEnergyToAll //only do this once
    |> reactToNeighbour
    |> resetAllLit

  let run =

    let firstRun : Grid =
      sampleInput
      |> List.mapi (fun rowIndex row -> row |> Seq.toList |> List.mapi (fun colIndex col -> rowIndex, colIndex, col |> charToInt))
      |> List.concat
      |> List.map ( fun (rowI, colI, value) -> (rowI, colI, value, false) : Row )

    [1..2]
    |> List.fold runThrough firstRun

