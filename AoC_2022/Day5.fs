namespace AdventOfCode
open System

module Day5 =

   let sampleInput = [
      "    [D]    "
      "[N] [C]    "
      "[Z] [M] [P]"
      " 1   2   3 "
      ""
      "move 1 from 2 to 1"
      "move 3 from 1 to 3"
      "move 2 from 2 to 1"
      "move 1 from 1 to 2"
   ]

   let input = TextFileReader.readFileContents "../../../Day5Input.txt" |> Seq.toList
   module Part1 =
      let run =
         // get the opening stack layout
         // which is all input up to the blank row..
         // now the remaining lines need to be split to have `count` , from and to.

         input
         |> List.splitOnceOnCharExcl (fun x -> x = "")
         |> fun (stack, instructions) ->
            let stackCount = stack |> List.rev |> List.head
            let stackPositions =
               stackCount
               |> Seq.toList
               |> List.mapi (fun i x -> // find the index of the stack
                  if x |> Char.IsNumber
                  then i
                  else 0
                  )
               |> List.filter (fun i -> i <> 0)

            let startingStack =
               stack
               |> List.rev
               |> List.skip 1// skip the header
               |> List.rev
               |> List.map (fun x ->
                  Seq.toList x
                  |> List.mapi (fun i c -> i,c )
                  |> List.filter (fun (i,c) ->
                     List.contains i stackPositions)
                  |> List.map (fun (_,c) -> c)
               )
               |> List.map (fun x ->
                  x)
               |> List.transpose

            let instructionSet =
               instructions
               |> List.map (fun instructionText ->
                  instructionText.Replace("move", "")
                                 .Replace(" from ", "-")
                                 .Replace(" to ","-")
                  |> String.splitMultipleOnCharExcl '-'
               )
               |> List.map (fun iL -> iL[0], iL[1], iL[2])

            let moveToNewStack ((from, dest) : int * int) (stack: List<List<Char>>) =
               // move 1 from to dest

               let fromStack =
                  stack[from - 1]
                  |> List.filter Char.IsLetter
               let toStack =
                  stack[dest - 1]
                  |> List.filter Char.IsLetter

               let charToMove =
                  fromStack
                  |> List.head
               let newFromStack =
                  fromStack |> List.tail
               let newToStack =
                  charToMove::toStack
               let newStack =
                  stack
                  |> List.mapi (fun index oldList ->
                     if index = from - 1 then
                        newFromStack
                     elif index = dest - 1 then
                        newToStack
                     else
                        oldList

                     )
               newStack

            let finalStack =
               instructionSet
               |> List.fold (fun stack (count, from, dest) ->
                  [1..(count |> int)]
                  |> List.fold
                     (fun stack i ->
                     moveToNewStack (from |> int ,dest |> int) stack) stack
                  ) startingStack
            finalStack
            |> List.iter (fun x ->
               printf $"{x[0]}")
            printfn " ..."
         ()

   module Part2 =
      let run =
         input
         |> List.splitOnceOnCharExcl (fun x -> x = "")
         |> fun (stack, instructions) ->
            let stackCount = stack |> List.rev |> List.head
            let stackPositions =
               stackCount
               |> Seq.toList
               |> List.mapi (fun i x -> // find the index of the stack
                  if x |> Char.IsNumber
                  then i
                  else 0
                  )
               |> List.filter (fun i -> i <> 0)

            let startingStack =
               stack
               |> List.rev
               |> List.skip 1// skip the header
               |> List.rev
               |> List.map (fun x ->
                  Seq.toList x
                  |> List.mapi (fun i c -> i,c )
                  |> List.filter (fun (i,c) ->
                     List.contains i stackPositions)
                  |> List.map (fun (_,c) -> c)
               )
               |> List.map (fun x ->
                  x)
               |> List.transpose

            let instructionSet =
               instructions
               |> List.map (fun instructionText ->
                  instructionText.Replace("move", "")
                                 .Replace(" from ", "-")
                                 .Replace(" to ","-")
                  |> String.splitMultipleOnCharExcl '-'
               )
               |> List.map (fun iL -> iL[0], iL[1], iL[2])

            let moveToNewStack ((count, from, dest) : int * int * int) (stack: List<List<Char>>) =
               // move 1 from to dest

               let fromStack =
                  stack[from - 1]
                  |> List.filter Char.IsLetter
               let toStack =
                  stack[dest - 1]
                  |> List.filter Char.IsLetter

               let charToMove =
                  fromStack
                  |> List.take count

               printfn $"Char to move: {charToMove}"
               let newFromStack =
                  fromStack |> List.skip count
               let newToStack =
                  charToMove@toStack
               let newStack =
                  stack
                  |> List.mapi (fun index oldList ->
                     if index = from - 1 then
                        newFromStack
                     elif index = dest - 1 then
                        newToStack
                     else
                        oldList

                     )
               newStack

            let finalStack =
               instructionSet
               |> List.fold (fun stack (count, from, dest) ->
                     moveToNewStack (count |> int, from |> int ,dest |> int) stack) startingStack
            finalStack
            |> List.iter (fun x ->
               printf $"{x[0]}")
            printfn " ..."
         ()

   let run =
      Part1.run
      Part2.run