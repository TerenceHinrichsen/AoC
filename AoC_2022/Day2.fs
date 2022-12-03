namespace AdventOfCode
module Day2 =

   let sampleInput = [
      "A Y"
      "B X"
      "C Z"
      ]

   type Pattern =
      | Rock
      | Paper
      | Scissors

   type Outcome =
     | Win
     | Lose
     | Draw

   let getScoreFromOutcome outcome =
      match outcome with
      | Win -> 6
      | Lose -> 0
      | Draw -> 3

   let getScoreFromPattern = function
      | Rock -> 1
      | Paper -> 2
      | Scissors -> 3

   let getPatternFromString s =
      match s with
      | "A" -> Rock //rock
      | "B" -> Paper //paper
      | "C" -> Scissors //scissors
      | "X"-> Rock //rock
      | "Y" -> Paper //paper
      | "Z" -> Scissors // scissors
      | _ -> failwith "Invalid pattern"

   let getOutcomeFromString = function
      | "X" -> Lose
      | "Y" -> Draw
      | "Z" -> Win
      | _ -> failwith "Invalid outcome"

   let calculateWinner (p1:Pattern) (p2:Pattern) =
      match p1, p2 with
      | Rock, Scissors -> Win
      | Rock, Paper -> Lose
      | Rock, Rock -> Draw
      | Paper, Rock -> Win
      | Paper, Scissors -> Lose
      | Paper, Paper -> Draw
      | Scissors, Paper -> Win
      | Scissors, Rock -> Lose
      | Scissors, Scissors -> Draw

   let calculateHandForExpectedOutcome opponentHand expectedOutcome =
      match expectedOutcome with
      | Win ->
         match opponentHand with
         | Rock -> Paper
         | Paper -> Scissors
         | Scissors -> Rock
      | Lose ->
         match opponentHand with
         | Rock -> Scissors
         | Paper -> Rock
         | Scissors -> Paper
      | Draw ->
         match opponentHand with
         | Rock -> Rock
         | Paper -> Paper
         | Scissors -> Scissors

   let getRoundPoints c1 c2 =
      let competitorHand = getPatternFromString c1
      let myHand = getPatternFromString c2

      let outcome = calculateWinner myHand competitorHand

      let outcomeScore = getScoreFromOutcome outcome
      let getHandScore = getScoreFromPattern myHand

      outcomeScore + getHandScore

   let calculatePuzzle1 (input: List<string>) =
      input
      |> List.map (fun s -> s.Split " ")
      |> List.map (fun s ->
         getRoundPoints s.[0] s.[1])
      |> List.sum
      |> printfn "Puzzle 1 : %A"

   let calculatePuzzle2 (input : List<string>) =
      input
      |> List.map (fun s -> s.Split " ")
      |> List.map (fun s ->
         let competitorHand = getPatternFromString s.[0]
         let expectedOutcome = getOutcomeFromString s.[1]
         s.[1], calculateHandForExpectedOutcome competitorHand expectedOutcome)
      |> List.map (fun (expectedOutcome, myHand) ->
         let outcomeScore =
            getScoreFromOutcome (getOutcomeFromString expectedOutcome)
            |> fun x -> printfn "Outcome score : %A" x; x
         let getHandScore =
            printfn $"My hand : {myHand}"
            getScoreFromPattern myHand
            |> fun x -> printfn "Hand score : %A" x ; x
         outcomeScore + getHandScore)
      |> List.sum
      |> printfn "Puzzle 2 : %A"





   let input = TextFileReader.readFileContents "../../../Day2Input.txt"
   let run =
      p "Day 2:"
      calculatePuzzle1 sampleInput
      calculatePuzzle1 (input |> Seq.toList)

      calculatePuzzle2 sampleInput
      calculatePuzzle2 (input |> Seq.toList)