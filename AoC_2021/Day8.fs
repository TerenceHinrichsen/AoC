module Day8

  let sampleInput = [
    "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"
    "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc"
    "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg"
    "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb"
    "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea"
    "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb"
    "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe"
    "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef"
    "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb"
    "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"
  ]

  let input = TextFileReader.readFileContents "../../../Day8Input.txt" |> Seq.toList

  let digit0 = "abcefg"
  let digit1 = "cf"
  let digit2 = "acdeg"
  let digit3 = "acdfg"
  let digit4 = "bcdf"
  let digit5 = "abdfg"
  let digit6 = "abdefg"
  let digit7 = "acf"
  let digit8 = "abcdefg"
  let digit9 = "abcdfg"

  let run =

    let puzzleInput =
      sampleInput
      |> List.map (fun x -> x.Split("|") |> Seq.toList)
      |> Seq.toList

    let output =
      puzzleInput
      |> List.map (List.skip 1)
      |> List.map List.head

    let sortedOutputs =
      output
      |> List.map (fun x -> x.Split(" ") |> Seq.toList |> List.skip 1)
      |> Seq.toList
      |> List.map (fun charGroup ->
        charGroup
        |> List.map (fun x ->
          x
          |> Seq.toList
          |> Seq.sort
          |> Seq.toArray
          |> System.String )
        |> fun x -> printfn $"{x}" ; x )

    let count1 =
      sortedOutputs
      |> List.concat
      |> List.filter (fun x -> x.Length = digit1.Length)
      |> List.length

    let count4 =
      sortedOutputs
      |> List.concat
      |> List.filter (fun x -> x.Length = digit4.Length)
      |> List.length
    let count7 =
      sortedOutputs
      |> List.concat
      |> List.filter (fun x -> x.Length = digit7.Length)
      |> List.length

    let count8 =
      sortedOutputs
      |> List.concat
      |> List.filter (fun x -> x.Length = digit8.Length)
      |> List.length

    printfn $"Total occurrences = {count1 + count4 + count7 + count8}"

    let createMapFromChar8 (char8:string) =
      char8
      |> Seq.toList
      |> Seq.mapi (fun i c -> (i, c))
      |> Seq.toList

    let decodeString (string: string) char8Map =
      string
      |> Seq.toList
      |> id


    let something =
      puzzleInput
      |> List.map (fun row -> row.Head, row.Tail |> List.head)
      |> List.map (fun (input, output) ->
        printfn $"Input : {input}"
        printfn $"Output : {output}"

        let char8 =
          input.Split(" ")
          |> Seq.toList
          |> List.find (fun x -> x.Length = 7)
        printfn $"Char 8 represented by : {char8}"

        let wireMap = createMapFromChar8 char8

        wireMap

      )

    something |> ignore
    ()
