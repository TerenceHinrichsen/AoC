module Day1

   open AdventOfCode
   open System
   open System.Text.RegularExpressions

   let sampleInput = [
      "two1nine"
      "eightwothree"
      "abcone2threexyz"
      "xtwone3four"
      "4nineeightseven2"
      "zoneight234"
      "7pqrstsixteen"
   ]

   let input =
      TextFileReader.readFileContents "../../../Day1Input.txt"


   let run =
      input
      |> List.map (fun s ->
         let numberList = s |> Seq.filter (fun x -> x |> Char.IsDigit)
         let firstNumber = numberList |> Seq.head
         let lastNumber = numberList |> Seq.last
         (firstNumber, lastNumber)
      )
      |> List.map (fun (c1,c2) -> $"{c1}{c2}" |> int)
      |> List.sum
      |> fun x -> printfn $"Day 1 Part 1: {x}"

      let numberToInt = function
         | "1" | "one"   -> 1
         | "2" | "two"   -> 2
         | "3" | "three" -> 3
         | "4" | "four" -> 4
         | "5" | "five" -> 5
         | "6" | "six"  -> 6
         | "7" | "seven" -> 7
         | "8" | "eight" -> 8
         | "9" | "nine" -> 9
         | x -> failwith $"Oops, regex broken- found {x}"

      let pattern = "(1|one|2|two|3|three|4|four|5|five|6|six|7|seven|8|eight|9|nine)"

      input
      |> List.map (fun s ->
         let first =
            Regex.Matches(s, pattern).[0].Value
         let last =
            Regex.Matches(s, pattern, RegexOptions.RightToLeft).[0].Value
         let one = first |> numberToInt
         let two = last |> numberToInt
         $"{one}{two}"
      )
      |> List.map int
      |> List.sum
      |> fun x -> printfn $"Day 1 Part 2: {x}"
