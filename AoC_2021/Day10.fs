module Day10

  let sampleInput = [
      "[({(<(())[]>[[{[]{<()<>>"
      "[(()[<>])]({[<{<<[]>>("
      "{([(<{}[<>[]}>{[]{[(<()>"
      "(((({<>}<{<{<>}{[]{[]{}"
      "[[<[([]))<([[{}[[()]]]"
      "[{[{({}]{}}([{[{{{}}([]"
      "{<[[]]>}<{[{[{[]{()[[[]"
      "[<(<(<(<{}))><([]([]()"
      "<{([([[(<>()){}]>(<<{{"
      "<{([{{}}[<[[[<>{}]]]>[]]"
    ]

  let input = TextFileReader.readFileContents "../../../Day10Input.txt" |> Seq.toList

  let run =

    let puzzleInput =
      input

    let validOpenChar char =
      ['('; '{'; '<'; '[']
      |> List.contains char

    let findOpeningCharForClosing char =
      match char with
      | ')' -> '('
      | '}' -> '{'
      | '>' -> '<'
      | ']' -> '['
      | x  -> failwith $"Invalid closing char: {x}"

    let findClosingCharForOpening char =
      match char with
      | '(' -> ')'
      | '{' -> '}'
      | '<' -> '>'
      | '[' -> ']'
      | x  -> failwith $"Invalid opening char: {x}"

    let rec validate (currentOpenChar : List<char>) charToCheckL invalidO =
      match invalidO with
      | Some c -> Some c
      | None ->
        match charToCheckL with
        | [] -> None
        | x::xs ->
          if validOpenChar x
          // add the opening to the currentOpenCharList
          then
            let nowCurrentOpenChar = currentOpenChar@[x]
            validate nowCurrentOpenChar xs None
          else
            // this is not an opening char, so we must check the last opening to see if it matches
            let matchingOpeningChar =
              findClosingCharForOpening (currentOpenChar |> List.rev |> List.head)

            if x = matchingOpeningChar then
              // this is valid, we should remove the char and return the remaining
                   validate (currentOpenChar |> List.rev |> List.tail |> List.rev) xs None
            else
              printfn $"Expecting {matchingOpeningChar}, but found {x} instead"
              validate [] xs (Some x)

    let scoreCard c =
      match c with
      | ')' ->  3
      | ']' ->  57
      | '}' ->  1197
      | '>' ->  25137
      | x -> failwithf $"Unexpected char {x}"

    let part1 =
      puzzleInput
      |> List.mapi (fun index charList ->
        let invalidChar = validate [] (charList |> List.ofSeq) None
        printfn $"Line {index} : {invalidChar}"
        Option.map scoreCard invalidChar )
      |> List.choose id
      |> List.sum
      |> printfn "Final score %A"

    let validLines =
      puzzleInput
      |> List.filter (fun charList ->
        let invalidChar = validate [] (charList |> List.ofSeq) None
        Option.isNone invalidChar )
      |> fun x -> printfn $"{x |> List.length} valid lines remain"; x

    let scoreCardP2 c =
      match c with
      | ')' ->  1
      | ']' ->  2
      | '}' ->  3
      | '>' ->  4
      | x -> failwithf $"Unexpected char {x}"

    let rec calculateMissing (currentOpenChar : List<char>) charToCheckL =
        match charToCheckL with
        | [] -> currentOpenChar |> List.rev
        | x::xs ->
          if validOpenChar x
          // add the opening to the currentOpenCharList
          then
            let nowCurrentOpenChar = currentOpenChar@[x]
            calculateMissing nowCurrentOpenChar xs
          else
            // this is not an opening char, so we must check the last opening to see if it matches
            let matchingOpeningChar =
              findClosingCharForOpening (currentOpenChar |> List.rev |> List.head)

            if x = matchingOpeningChar then
              // this is valid, we should remove the char and return the remaining
                   calculateMissing (currentOpenChar |> List.rev |> List.tail |> List.rev) xs
            else
              printfn $"Expecting {matchingOpeningChar}, but found {x} instead"
              calculateMissing [] xs

    let calculateScore (startingScore: int64) char =
      let charScore = scoreCardP2 (char |> findClosingCharForOpening) |> int64
      let closingScore = (startingScore * 5L) + charScore
      printfn $"Starting {startingScore} : Ending {closingScore} : CharScore {charScore}"
      closingScore

    let part2 =
      let sortedResults =
        validLines
        |> List.mapi (fun index charList ->
          let x = calculateMissing [] (charList |> List.ofSeq)
          printfn $"Line {index} returned {x}"
          x
          |> List.fold calculateScore 0L )
        |> List.sort

      let halfway = (sortedResults |> List.length) / 2
      sortedResults
      |> List.skip halfway
      |> List.head
      |> printfn "Part 2: %A"



    part2