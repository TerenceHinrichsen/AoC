module Day3

   open System.Text.RegularExpressions
   open AdventOfCode
   open System

   let sampleInput = [
      "467..114.."
      "...*......"
      "..35..633."
      "......#..."
      "617*......"
      ".....+.58."
      "..592....."
      "......755."
      "...$.*...."
      ".664.598.."
   ]

   let input =
      TextFileReader.readFileContents "../../../Day3Input.txt"

   let getAdjacentCells cellPos =
      let rowAbove = fst cellPos - 1
      let rowBelow = fst cellPos + 1
      let columnLeft = snd cellPos - 1
      let columnRight = snd cellPos + 1
      let adjacentCells = [
         (rowAbove, columnLeft)
         (rowAbove, snd cellPos)
         (rowAbove, columnRight)
         (fst cellPos, columnLeft)
         (fst cellPos, columnRight)
         (rowBelow, columnLeft)
         (rowBelow, snd cellPos)
         (rowBelow, columnRight)
         ]
      adjacentCells

   type SingleValue =
      | Empty
      | Symbol
      | SingleDigitNumber of int

   type ElementInGrid = {
      xPos: int
      yPos: int
      value: SingleValue
   }

   let processGridToElementList (input: List<string>) =
      input
      |> List.mapi (fun xPos row ->
         row
         |> Seq.mapi (fun yPos cell ->
            {
               xPos = xPos
               yPos = yPos
               value = // first run will only have single digit numbers
                  match cell with
                  | '.' -> Empty
                  | '1' -> SingleDigitNumber 1
                  | '2' -> SingleDigitNumber 2
                  | '3' -> SingleDigitNumber 3
                  | '4' -> SingleDigitNumber 4
                  | '5' -> SingleDigitNumber 5
                  | '6' -> SingleDigitNumber 6
                  | '7' -> SingleDigitNumber 7
                  | '8' -> SingleDigitNumber 8
                  | '9' -> SingleDigitNumber 9
                  | '0' -> SingleDigitNumber 0
                  | _ -> Symbol // anything that is not a fullstop or a number should be a symbol.
            }
         )
      |> Seq.toList
      )
      |> List.concat

   let processRowToNumberL (inputL: List<ElementInGrid>) =

      let rec buildNumbersRec (inputL: List<ElementInGrid>) (currentNumber: int) (currentNumberL: List<int * int>) =
         match inputL with
         | [] -> currentNumberL
         | head::tail ->
            match head.value with
            | SingleDigitNumber x ->
               buildNumbersRec tail (currentNumber * 10 + x) currentNumberL
            | _ ->
               let updatedList =
                  if currentNumber > 0 then currentNumberL @ [head.yPos, currentNumber] else currentNumberL
               buildNumbersRec tail 0 updatedList
      buildNumbersRec inputL 0 []

   let checkAdjacentCellsForSymbols (completeM: List<ElementInGrid>) (numberL: List<int * List<int * int>>) =
      numberL
      |> List.map (fun (rowId, numbers) ->
         numbers
         |> List.map (fun (columnId, number) ->
            let numberLength = number.ToString().Length
            let adjacentCells =
               [columnId-numberLength ..columnId - 1]
               |> List.map (fun colId ->
                  getAdjacentCells (rowId, colId))
               |> List.concat

            let adjacentSymbols =
               adjacentCells
               |> List.filter (fun (rowId, columnId) ->
                  completeM
                  |> List.filter (fun x ->
                     x.xPos = rowId && x.yPos = columnId)
                  |> List.map (fun x -> x.value)
                  |> List.exists (fun x -> x = Symbol)
               )
               |> id
            if adjacentSymbols.Length > 0 then
               number
            else
               0
         )
         )
      |> List.concat

   let parseInputToAnswer (input : List<string>) =
      let completeMap =
         input
         |> processGridToElementList
      let numbersWithSymbolsAdjacent  =
         completeMap
         |> List.groupBy (fun x -> x.xPos)
         |> List.map (fun (rowId, elementsInRow) ->
            let result =
               elementsInRow
               |> processRowToNumberL
               |> List.filter (fun (_x,y) -> y <> 0)
            // now i have the position and length of each number in the row
            rowId, result
            )
         |> checkAdjacentCellsForSymbols completeMap
      numbersWithSymbolsAdjacent
      |> List.map(fun x ->
         printfn $"Found: {x}"
         x)
      |> List.sum
      |> fun x -> printfn $"Part 1 Answer: {x}"

   let run =
      input
      |> parseInputToAnswer
