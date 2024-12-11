module Day9

   open AdventOfCode
   open System
   open System.Text.RegularExpressions
   open FSharpPlus

   let sampleInput = seq [
      "2333133121414131402"
   ]

   let input =
      TextFileReader.readFileContents "../../../Day9Input.txt"

   type Structure = {
      FileId : int
      NumberOfBlocks : int
      FreeSpace : int
   }

   let run =

      let formattedInput =
         sampleInput
         |> Seq.concat
         |> Seq.toList

      let xx =
         formattedInput
         |> List.chunkBySize 2
         |> List.mapi (fun index x -> { FileId = index; NumberOfBlocks = x[0] |> int; FreeSpace = x[1] |> int } )

      let findFirstFreeSpace (disk: List<Structure>) = disk |> List.tryFind (fun x -> x.FreeSpace > 0)

      let rec step newDisk =
         newDisk
         |> List.foldBack (fun current (acc: List<Structure>) ->
            match acc with
            | [] -> acc
            | next :: rest ->
               printfn $"Current: {next}"
               printfn $"Next: {current}"
               let revAcc = acc |> List.rev
               match findFirstFreeSpace revAcc with
               | Some firstFreeSpace ->
                  let newFreeSpace = { firstFreeSpace with FreeSpace = firstFreeSpace.FreeSpace - 1 }
                  let newCurrent = { current with NumberOfBlocks = current.NumberOfBlocks - 1 }
                  let newAcc =
                     rest |> List.map (fun x -> if x.FileId = firstFreeSpace.FileId then newFreeSpace else x)
                  newAcc
               | None -> acc
         )

      step xx
