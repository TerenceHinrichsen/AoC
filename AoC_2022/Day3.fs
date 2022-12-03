namespace AdventOfCode

module Day3 =

   let sampleInput = [
      "vJrwpWtwJgWrhcsFMMfFFhFp"
      "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
      "PmmdzqPrVvPwwTWBwg"
      "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
      "ttgJtRGJQctTZtZT"
      "CrZsJsPPZsGzwwsLwLmpwMDw"
      ]

   let input = TextFileReader.readFileContents "../../../Day3Input.txt"
   let run =
      p "Day 3:"

      //Lowercase item types a through z have priorities 1 through 26.
      // Uppercase item types A through Z have priorities 27 through 52.
      let calculatePriority l =
         let scoreCalc = [
            'a',1
            'b',2
            'c',3
            'd',4
            'e',5
            'f',6
            'g',7
            'h',8
            'i',9
            'j',10
            'k',11
            'l',12
            'm',13
            'n',14
            'o',15
            'p',16
            'q',17
            'r',18
            's',19
            't',20
            'u',21
            'v',22
            'w',23
            'x',24
            'y',25
            'z',26
            'A',27
            'B',28
            'C',29
            'D',30
            'E',31
            'F',32
            'G',33
            'H',34
            'I',35
            'J',36
            'K',37
            'L',38
            'M',39
            'N',40
            'O',41
            'P',42
            'Q',43
            'R',44
            'S',45
            'T',46
            'U',47
            'V',48
            'W',49
            'X',50
            'Y',51
            'Z',52
         ]
         let scoreM =
            scoreCalc
            |> Map.ofList
         scoreM
         |> Map.find l

      input
      |> Seq.toList
      |> Seq.map (fun sackContents ->
         sackContents
         |> Seq.toList
         |> List.splitInto 2
         |> List.pairwise
         |> List.map (fun (c1, c2) ->
            c1 |> List.map (fun x ->
               c2
               |> List.tryFind (fun c -> c = x)
               )
            |> List.choose id
            |> List.distinct
            )
         )
      |> Seq.map (fun x ->
         x
         |> List.map (fun y ->
            y
            |> List.map (fun z ->
               calculatePriority z
               )
            |> List.sum
            )
         |> List.sum
         )
      |> Seq.sum
      |> fun x -> p $"Part 1 total priorities are: {x}"

      input
      |> Seq.toList
      |> List.chunkBySize 3
      |> List.map (fun grouping ->
         let e1 = grouping.[0] |> Seq.toList
         let e2 = grouping.[1] |> Seq.toList
         let e3 = grouping.[2] |> Seq.toList

         e1
         |> List.choose (fun x ->
            let e1Ande2 =
               e2
               |> List.tryFind (fun y -> y = x)
            let firstTwoAnde3 =
               e1Ande2
               |> Option.bind (fun zz ->
                  e3
                  |> List.tryFind (fun z -> z = zz)
                  )
            firstTwoAnde3)
         |> List.distinct
         |> List.map (fun x ->
            x
            |> calculatePriority
            )
         )
      |> List.map (fun iL -> iL |> List.sum)
      |> List.sum
      |> fun x -> printfn $"Part 2 total priorities are : {x}"