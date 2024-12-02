module Day11

   open AdventOfCode
   open System
   open System.Text.RegularExpressions
   open FsToolkit.ErrorHandling

   let sampleInput = [
         "...#......"
         ".......#.."
         "#........."
         ".........."
         "......#..."
         ".#........"
         ".........#"
         ".........."
         ".......#.."
         "#...#....."
      ]

   let input =
      TextFileReader.readFileContents "../../../Day11Input.txt"

   let expandEmptySpaces (input: List<List<char>>) =
      ()

   let inputToArray (input: List<string>) : List<List<char>> =
      input |> List.map (fun x -> x |> Seq.toList)

   let isRowEmpty (row: List<char>) =
      row
      |> List.filter (fun x -> x = '#')
      // so if the list is not empty, it is not an empty row
      |> List.isEmpty

   let isColumnEmpty (column: List<char>) =
      column |> List.filter (fun x -> x = '#')
      |> List.isEmpty

   let parseInputToAnswer (input: List<string>) =
      let startingMap =  sampleInput |> inputToArray
      let rowL =
         startingMap
         |> List.mapi (fun index row -> index, row)
      let columnL =
         startingMap
         |> List.transpose // columnList
         |> List.mapi (fun index column -> index, column)

      let rowL =
         rowL
         |> List.map (fun (x,y) ->
            x,
            if isRowEmpty y then
               [y; y]
            else [y]
            ) // expand empty rows
         |> List.map (fun (x,y) -> x, y |> List.concat)

      let columnL =
         columnL
         |> List.map (fun (x,y) -> x, y, isColumnEmpty y)

      rowL
      |> List.iter (fun (rowIndex, rowValues, isEmpty) -> printfn $"Row    {rowIndex} Contains: {rowValues}: IsEmpty {isEmpty}")

      columnL
      |> List.iter (fun (columnIndex,columnValues, isEmpty) -> printfn $"Column {columnIndex} Contains: {columnValues}: IsEmpty: {isEmpty}")


   let run =
      parseInputToAnswer input
