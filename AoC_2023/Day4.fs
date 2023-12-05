module Day4

   open AdventOfCode
   open System
   open System.Text.RegularExpressions

   let sampleInput = [
      "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
      "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
      "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
      "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
      "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
      "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"   ]

   let input =
      TextFileReader.readFileContents "../../../Day4Input.txt"

   type ScratchCard =
      {
         CardNumber : string
         WinningNumbers: List<int>
         MyNumbers: List<int>
      }


   let run =

      let findCountOfWinningNumbers (s : ScratchCard) =
         s.WinningNumbers
         |> List.filter (fun n ->
            s.MyNumbers
            |> List.contains n
         )
         |> List.length

      let processInputToScratchCardL (input: List<string>) =
         input
         |> List.map (fun line ->
            let x = line.Split ":"
            Array.iter (fun x -> printfn $"{x}") |> ignore
            let cardNumber = x |> Array.head
            let numbers = x |> Array.tail |> Array.head
            let winningNumbers =
               numbers.Split " | "
               |> Array.head
               |> fun x -> x.Trim().Split " "
               |> Array.filter (fun x -> x <> "")
               |> Array.map int
               |> Seq.toList
            let myNumbers =
               numbers.Split " | "
               |> Array.tail
               |> Array.head
               |> fun x -> x.Trim().Split " "
               |> Array.filter (fun x -> x <> "")
               |> Array.map (fun x -> x |> int  )
               |> Seq.toList
            {
               CardNumber =  cardNumber
               WinningNumbers = winningNumbers
               MyNumbers = myNumbers
            }
         )

      let scratchCardL = input |> processInputToScratchCardL

      scratchCardL
      |> List.map (fun sc -> sc.CardNumber, findCountOfWinningNumbers sc )
      |> List.map (fun (_cardNumber, x) ->
         if x <= 0 then 0 else  1 <<< (x - 1) )
      |> List.sum
      |> fun x -> printfn $"Day 4: Part 1: total score: {x}"

      //ROUND 2:
      let scratchCardL =
         sampleInput
         |> processInputToScratchCardL

      scratchCardL
      |> List.mapi (fun i sc -> i+1, sc.CardNumber, findCountOfWinningNumbers sc )
//      |> List.iter (fun (x,y,z) -> printfn $"{y} ({x}) has {z} winning numbers")
      |> List.map (fun (cardNumber, _name, countOfWinningNumbers) ->
         let remainingDeck =
            scratchCardL
            |> List.splitAt cardNumber
            |> snd
         remainingDeck
         |> List.take countOfWinningNumbers
         )
      |> List.concat
      |> List.groupBy (fun sc -> sc.CardNumber)
      |> List.iter (fun (cardNumber, scL) ->
         scL
         |> List.iter (fun sc -> printfn $"{cardNumber} contains {sc.WinningNumbers}")
         )
