module Day6

   open AdventOfCode
   open System
   open System.Text.RegularExpressions
   open FSharpPlus

   let sampleInput = [
      "....#....."
      ".........#"
      ".........."
      "..#......."
      ".......#.."
      ".........."
      ".#..^....."
      "........#."
      "#........."
      "......#..."
   ]

   let input =
      TextFileReader.readFileContents "../../../Day6Input.txt"

   type Direction =
      | Up
      | Down
      | Left
      | Right
      with
         static member fromString s =
            match s with
            | "^" -> Up
            | "v" -> Down
            | "<" -> Left
            | ">" -> Right
            | _ -> failwith "Invalid direction"
         member this.rotateRight =
            match this with
            | Up -> Right
            | Right -> Down
            | Down -> Left
            | Left -> Up

   type Cell = {
      X: int
      Y: int
      Char: char
      HasBeenVisited: bool
      IsObstacle: bool
      CurrentDirection: Option<Direction>
   }
   with
      member this.Coordinates = (this.X, this.Y)
      member this.ApplyMovement movement =
         match movement with
         | Direction.Up -> { this with X = this.X - 1 }
         | Direction.Down -> { this with X = this.X + 1 }
         | Direction.Left -> { this with Y = this.Y - 1 }
         | Direction.Right -> { this with Y = this.Y + 1 }

   let run =

      let map =
         input
         |> List.mapi (fun x s ->
            Seq.toList s
            |> List.mapi (fun y c ->
               match c with
               | '.' -> { X = x; Y = y; Char = c; HasBeenVisited = false; IsObstacle = false; CurrentDirection = None }
               | '#' -> { X = x; Y = y; Char = c; HasBeenVisited = false; IsObstacle = true ; CurrentDirection = None }
               | 'X' -> { X = x; Y = y; Char = c; HasBeenVisited = true; IsObstacle = false ; CurrentDirection = None }
               | '^' -> { X = x; Y = y; Char = c; HasBeenVisited = true; IsObstacle = false; CurrentDirection = Some Direction.Up }
               | 'v' -> { X = x; Y = y; Char = c; HasBeenVisited = true; IsObstacle = false; CurrentDirection = Some Direction.Down }
               | '<' -> { X = x; Y = y; Char = c; HasBeenVisited = true; IsObstacle = false; CurrentDirection = Some Direction.Left }
               | '>' -> { X = x; Y = y; Char = c; HasBeenVisited = true; IsObstacle = false; CurrentDirection = Some Direction.Right }
               | x -> failwith $"Unknown {x} character found"
               )
            )
            |> List.concat

      let findStartingPosition = map |> List.filter (fun x -> x.CurrentDirection.IsSome) |> List.head

      let checkIfCurrentPositionIsOutsideMap (currentPos: Cell) (map: List<Cell>) =
         let x,y = currentPos.Coordinates
         let maxX = map |> List.maxBy (fun x -> x.X) |> fun x -> x.X
         let maxY = map |> List.maxBy (fun x -> x.Y) |> fun x -> x.Y
         let minX = 0
         let minY = 0
         if x < minX || x > maxX || y < minY || y > maxY then
            true
         else
            false

      let rec move (currentPos: Cell) (currentMap: List<Cell>) =

         if checkIfCurrentPositionIsOutsideMap currentPos currentMap then
            printfn $"Position {currentPos.Coordinates} is outside the map"
            currentMap // return the map
         else

            let nextCellAddress =
               let direction = currentPos.CurrentDirection.Value
               (currentPos.ApplyMovement direction).Coordinates

            let nextCellO =
               currentMap |> List.tryFind (fun x -> x.Coordinates = nextCellAddress)

            match nextCellO with
            | Some nextCell ->


               if nextCell.IsObstacle then
                  let newDirection = currentPos.CurrentDirection.Value.rotateRight
                  let newCell = { currentPos with CurrentDirection = Some newDirection }
                  let updatedMap = currentMap |> List.map (fun x -> if x = currentPos then newCell else x)
                  move newCell updatedMap
               else
                  let newCell = { nextCell with HasBeenVisited = true; CurrentDirection = currentPos.CurrentDirection }
                  let updatedMap =
                     currentMap |> List.map (fun x -> if x.Coordinates = nextCell.Coordinates then newCell else x)
                  move newCell updatedMap

            | None -> currentMap

      let finalMap =
         printfn $"Starting position: {findStartingPosition.Coordinates}"
         move findStartingPosition map
      let numberOfCellsVisited = finalMap |> List.filter (fun x -> x.HasBeenVisited) |> List.length

      printfn $"Part1 = Visited cells: {numberOfCellsVisited}"
