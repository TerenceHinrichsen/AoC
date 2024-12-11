module Day10

   open AdventOfCode
   open System
   open System.Text.RegularExpressions
   open FSharpPlus

   let sampleInput = seq [
      "89010123"
      "78121874"
      "87430965"
      "96549874"
      "45678903"
      "32019012"
      "01329801"
      "10456732"
   ]

   let input =
      TextFileReader.readFileContents "../../../Day10Input.txt"

   type Node = {
      XPos: int
      YPos: int
      Value: int
      HasBeenVisited: bool
   }

   let run =

      let startingMap =
         input
         |> Seq.toList
         |> List.mapi (fun i x ->
            x.ToCharArray()
            |> Array.toList
            |> List.mapi (fun j x ->
               {  XPos = i
                  YPos = j
                  Value = int (string x)
                  HasBeenVisited = false
               }
            )
         )
         |> List.concat

      let startingPositions = startingMap |> List.filter (fun x -> x.Value = 0)

      printfn $"Found {startingPositions.Length} starting positions"

      let directions = [(-1, 0); (1, 0); (0, -1); (0, 1)]
      let isInBounds (x, y) = x >= 0 && y >= 0 && x < input.Length && y < input.Head.Length

      let dijkstra trailhead =
         let rec explore queue visited =
            match queue with
            | [] -> visited
            | (cost, (x, y)) :: rest ->
               if Set.contains (x, y) visited then
                  explore rest visited
               else
                  let currentNode =
                     startingMap |> List.find (fun n -> n.XPos = x && n.YPos = y)
                  // Find valid neighbors
                  let neighbors =
                     directions
                     |> List.map (fun (dx, dy) -> (x + dx, y + dy))
                     |> List.filter (fun (nx, ny) ->
                        isInBounds (nx, ny) &&
                        not (Set.contains (nx, ny) visited)
                     )
                     |> List.choose (fun (nx, ny) ->
                        match startingMap |> List.tryFind (fun n -> n.XPos = nx && n.YPos = ny) with
                        | Some neighborNode when neighborNode.Value = currentNode.Value + 1 -> Some (nx, ny)
                        | _ -> None
                     )                  // Add neighbors to the queue
                  let updatedQueue =
                     neighbors
                     |> List.fold (fun acc (nx, ny) ->
                        let nextCost = cost + 1
                        (nextCost, (nx, ny)) :: acc
                     ) rest
                  explore updatedQueue (Set.add (x, y) visited)

         explore [(0, (trailhead.XPos, trailhead.YPos))] Set.empty


      let trailheadScores =
         startingPositions
         |> List.map (fun trailhead ->
            let reachableNodes = dijkstra trailhead
            reachableNodes
            |> Set.filter (fun (x, y) ->
               let node = startingMap |> List.find (fun n -> n.XPos = x && n.YPos = y)
               node.Value = 9
            )
            |> Set.count
         )
         |> List.sum

      printfn $"Total score of all trailheads: {trailheadScores}"

      let rec findPaths (x, y) currentHeight acc =
         match Map.tryFind ((x, y), currentHeight) acc with
         | Some count -> count, acc
         | None ->
            let currentNode =
               startingMap |> List.find (fun n -> n.XPos = x && n.YPos = y)

            if currentNode.Value = 9 then 1, acc
            else
               let neighbors =
                  directions
                  |> List.map (fun (dx, dy) -> (x + dx, y + dy))
                  |> List.filter isInBounds
                  |> List.choose (fun (nx, ny) ->
                     match startingMap |> List.tryFind (fun n -> n.XPos = nx && n.YPos = ny) with
                     | Some neighborNode when neighborNode.Value = currentHeight + 1 -> Some (nx, ny)
                     | _ -> None
                  )

               let totalPaths, updateAcc =
                  neighbors
                  |> List.fold (fun (accPaths, acc) neighbor ->
                     let paths, newAcc = findPaths neighbor (currentHeight + 1) acc
                     accPaths + paths, newAcc
                  ) (0, acc)

               totalPaths, Map.add ((x, y), currentHeight) totalPaths updateAcc

      let trailheadRatings =
         startingPositions
         |> List.fold (fun (totalRating, acc) trailhead ->
            let paths, updatedAcc = findPaths (trailhead.XPos, trailhead.YPos) 0 acc
            totalRating + paths, updatedAcc
         ) (0, Map.empty)
         |> fst

      printfn $"Part 2: {trailheadRatings}"
