module Day7

   open AdventOfCode
   open System
   open System.Text.RegularExpressions
   open FSharpPlus

   let sampleInput = seq [
      "190: 10 19"
      "3267: 81 40 27"
      "83: 17 5"
      "156: 15 6"
      "7290: 6 8 6 15"
      "161011: 16 10 13"
      "192: 17 8 14"
      "21037: 9 7 18 13"
      "292: 11 6 16 20"
   ]

   let input =
      TextFileReader.readFileContents "../../../Day7Input.txt"

   let run =

      let testsToRun =
         input
         |> Seq.toList
         |> List.map (fun x ->
            let testValue =
               x.Split(": ") |> Array.toList |> List.head |> int64
            let xL =
               x.Split(": ")
               |> Array.toList
               |> List.tail
            let equation =
               xL
               |> List.map (fun x -> x.Split " " |> Array.toList)
               |> List.concat
               |> List.map int64
            testValue, equation
            )


      // for every element in the list we want to get the pairs
      // apply add and multiply and then store the result
      // then we take the next element and apply the add / multiply and repeat
      // until there are no more elements left.
      let rec calculateOptions (listOfInputs: List<int64>) (acc: List<int64>) =
         printfn $"Calculating options for {listOfInputs}"
         printfn $"Already have : {acc}"
         match listOfInputs with
         | [] ->
            printfn $"Nothing left results are {acc}"
            acc
         | [head] ->
            let result =
               acc
               |> List.map (fun previousResult ->
                  [head * previousResult; head + previousResult] @ acc
                  )
               |> List.concat
            printfn $"Last element {head} results are {result}"
            result

         | head::tail ->
            printfn $"Working on {head} with {tail} left"
            let result =
               acc
               |> List.map (fun previousResult ->
                  [head * previousResult; head + previousResult] @ acc
                  )
               |> List.concat
            calculateOptions tail result

      let result =
         testsToRun
         |> List.map (fun (expectedAnswer, listOfInputs) ->
            let options = calculateOptions listOfInputs [0]
            expectedAnswer, options
         )
         |> List.filter (fun (expectedAnswer, answerL) ->
            answerL
            |> List.filter (fun x -> x = expectedAnswer)
            |> List.length > 0
            )
         |> List.map fst
         |> List.sum
         |> printfn "Part 1: %A"

      result
