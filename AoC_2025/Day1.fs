module Day1

   open AdventOfCode
   open System

   let sampleInput = [
      "L68"
      "L30"
      "R48"
      "L5"
      "R60"
      "L55"
      "L1"
      "L99"
      "R14"
      "L82"
   ]

   let input =
      TextFileReader.readFileContents "../../../Day1Input.txt"


   type Direction = | Right | Left

   // max position is 99 and min is 0, so wrapping around
   // skelms, in the actual, the count can exceed 100 :(
   let applyTurn currentPosition direction count =
      match direction with
      | Right when count < 100 ->
         if currentPosition + count > 99 then (currentPosition + count) - 100 else currentPosition + count
      | Right ->
         // now I know the count > 100
         let effectiveCount = count % 100 // because we go full circle at 100.
         if currentPosition + effectiveCount > 99 then (currentPosition + effectiveCount) - 100 else currentPosition + effectiveCount
      | Left when count < 100 ->
         if currentPosition - count < 0 then 100 + (currentPosition - count) else currentPosition - count
      | Left ->
         let effectiveCount = count % 100 // because we go full circle at 100.
         if currentPosition - effectiveCount < 0 then 100 + (currentPosition - effectiveCount) else currentPosition - effectiveCount

   let applyTurnAndCountPassesThroughZero currentPosition direction count =
      match direction with
      | Right when count < 100 ->
         let newPosition =
            if currentPosition + count > 99 then (currentPosition + count) - 100 else currentPosition + count
         let passesThroughZero =
            if currentPosition + count > 99 then 1 else 0
         (newPosition, passesThroughZero)
      | Right ->
         // now I know the count > 100
         let effectiveCount = count % 100 // because we go full circle at 100.
         let numberOfRotations = count / 100
         let newPosition =
            if currentPosition + effectiveCount > 99 then (currentPosition + effectiveCount) - 100 else currentPosition + effectiveCount
         let passesThroughZero =
            if currentPosition + effectiveCount > 99 then 1 + numberOfRotations else numberOfRotations
         (newPosition, passesThroughZero)
      | Left when count < 100 ->
         let newPosition =
            if currentPosition - count < 0 then 100 + (currentPosition - count) else currentPosition - count
         let passesThroughZero =
            if currentPosition - count < 0 then 1 else 0
         (newPosition, passesThroughZero)
      | Left ->
         let effectiveCount = count % 100 // because we go full circle at 100.
         let numberOfRotations = count / 100
         let newPosition =
            if currentPosition - effectiveCount < 0 then 100 + (currentPosition - effectiveCount) else currentPosition - effectiveCount
         let passesThroughZero =
            if currentPosition - effectiveCount < 0 then 1 + numberOfRotations else numberOfRotations
         (newPosition, passesThroughZero)


   let run =
      printfn "Part 1: sample "

      sampleInput
      |> List.map (fun s ->
         (if s[0] = 'L' then Left else Right), s[1..] |> int)
      // but we need to know how many times we end up on 0, so we have to add an accumulator
      |> List.fold (fun (position, zeroCount) (direction, count) ->
         let newPosition = applyTurn position direction count
         //printfn $"Moved from {position} to {newPosition} by turning {direction} and moving {count}"
         let newZeroCount = if newPosition = 0 then zeroCount + 1 else zeroCount
         (newPosition, newZeroCount) ) (50, 0)
      |> printfn "%A"

      printfn "***** Part 1: actual ****"

      input
      |> List.map (fun s ->
         (if s[0] = 'L' then Left else Right), s[1..] |> int)
      // but we need to know how many times we end up on 0, so we have to add an accumulator
      |> List.fold (fun (position, zeroCount) (direction, count) ->
         let newPosition = applyTurn position direction count
         //printfn $"Moved from {position} to {newPosition} by turning {direction} and moving {count}"
         let newZeroCount = if newPosition = 0 then zeroCount + 1 else zeroCount
         (newPosition, newZeroCount) ) (50, 0)
      |> printfn "%A"

      printfn "Part 2: sample "
      // so now we have to count how many times we go past 0, not just end up on 0
      sampleInput
      |> List.map (fun s ->
         (if s[0] = 'L' then Left else Right), s[1..] |> int)
      // now use the new apply function that counts passes through zero and accumulate that
      |> List.fold (fun (position, zeroPassCount) (direction, count) ->
         let newPosition, passesThroughZero = applyTurnAndCountPassesThroughZero position direction count
         //printfn $"Moved from {position} to {newPosition} by turning {direction} and moving {count}, passing through zero {passesThroughZero} times"
         let newZeroPassCount = zeroPassCount + passesThroughZero
         (newPosition, newZeroPassCount) ) (50, 0)
      |> printfn "%A"

      printfn "Part 2: actual "
      // so now we have to count how many times we go past 0, not just end up on 0
      input
      |> List.map (fun s ->
         (if s[0] = 'L' then Left else Right), s[1..] |> int)
      // now use the new apply function that counts passes through zero and accumulate that
      |> List.fold (fun (position, zeroPassCount) (direction, count) ->
         let newPosition, passesThroughZero = applyTurnAndCountPassesThroughZero position direction count
         printfn $"Moved from {position} to {newPosition} by turning {direction} and moving {count}, passing through zero {passesThroughZero} times"
         let newZeroPassCount = zeroPassCount + passesThroughZero + if newPosition = 0 then 1 else 0
         (newPosition, newZeroPassCount) ) (50, 0)
      |> printfn "%A"
