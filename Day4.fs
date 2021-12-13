module Day4

  let sampleInput = [
    "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1"
    ""
    "22 13 17 11  0"
    " 8  2 23  4 24"
    "21  9 14 16  7"
    " 6 10  3 18  5"
    " 1 12 20 15 19"
    "              "
    " 3 15  0  2 22"
    " 9 18 13 17  5"
    "19  8  7 25 23"
    "20 11 10 24  4"
    "14 21 16 12  6"
    "              "
    "14 21 17 24  4"
    "10 16 15  9 19"
    "18  8 23 26 20"
    "22 11 13  6  5"
    " 2  0 12  3  7"
  ]

  let input = TextFileReader.readFileContents "../../../Day4Input.txt" |> Seq.toList

  /// each board contains a list of rows and columns - 2d array?
  type block = int * bool
  type row = List<block>
  type board = List<row> * List<row>

  let isWinningRow (row: row) = row |> List.filter (fun block -> block |> snd) |> List.length = 5

  let markBlockAsFound (number: int) (row: row)  =
    row |> List.map (fun block -> if block |> fst = number then (number , true) : block else block )

  let run =
    let numberSequence =
      input
      |> List.head
      |> fun l -> l.Split(",")
      |> Seq.toList
      |> List.map int

    let boards =
      input
      |> List.skip 1
      |> List.chunkBySize 6
      |> List.map (List.skip 1)
      |> List.map (List.map (fun x ->
        x.Split(" ")
        |> Seq.filter (fun x -> x <> "")
        |> Seq.toList |> List.map int
        |> List.map (fun x -> (x, false) : block ) : row ))
      |> List.map (fun rowL -> (rowL, rowL |> List.transpose))
      : List<board>

    let checkIfBoardIsWinner (board: board) =
      let (rowL, colL) = board
      let winningRowO = rowL |> List.tryFind isWinningRow
      let winningColO = colL |> List.tryFind isWinningRow
      winningColO.IsSome || winningRowO.IsSome

    let calculateBoardScore (board: board) =
      let (rowL, _colL) = board
      let rowScore =
        rowL
        |> List.map (fun row ->
          row
          |> List.filter (fun block -> not (block |> snd))
          |> List.map (fun block -> block |> fst)
        |> List.sum)
        |> List.sum
      rowScore

    let rec markBoardsP1 (markedBoards: List<board>) (remainingNumbers: List<int>) : List<board> =
      printfn $"No winners yet, marking next number"
      match remainingNumbers with
      | [] -> markedBoards
      | [x] ->
        printfn $"Checking {x}"
        let newMarkedBoards =
          markedBoards
          |> List.map (fun board -> board |> fst |> List.map (markBlockAsFound x), (board |> snd |> List.map (markBlockAsFound x)): board)
        let winningBoard = newMarkedBoards |> List.tryFind checkIfBoardIsWinner
        if winningBoard.IsSome then
          printfn $"We have a winner!! {winningBoard}"
          let winningBoardScore = calculateBoardScore winningBoard.Value
          printfn $"Winning score is : {winningBoardScore}"
          newMarkedBoards
        else newMarkedBoards
      | x::xs ->
        printfn $"Checking {x}"
        let newMarkedBoards =
          markedBoards
          |> List.map (fun board -> board |> fst |> List.map (markBlockAsFound x), (board |> snd |> List.map (markBlockAsFound x)): board)
        let winningBoard = newMarkedBoards |> List.tryFind checkIfBoardIsWinner
        if winningBoard.IsSome then
          printfn $"We have a winner!! {winningBoard}"
          let winningBoardScore = calculateBoardScore winningBoard.Value
          printfn $"Winning score is : {winningBoardScore}"
          printfn $"Winning number was : {x}"
          printfn $"Final score: {winningBoardScore * x}"
          newMarkedBoards
        else markBoardsP1 newMarkedBoards xs

    let rec markBoardsP2 (markedBoards: List<board>) (remainingNumbers: List<int>) : List<board> =
      printfn $"No winners yet, marking next number"
      match remainingNumbers with
      | [] -> markedBoards
      | [x] ->
        printfn $"Checking {x}"
        let newMarkedBoards =
          markedBoards
          |> List.map (fun board -> board |> fst |> List.map (markBlockAsFound x), (board |> snd |> List.map (markBlockAsFound x)): board)
        newMarkedBoards

      | x::xs ->
        printfn $"Checking {x}"
        let newMarkedBoards =
          markedBoards
          |> List.map (fun board -> board |> fst |> List.map (markBlockAsFound x), (board |> snd |> List.map (markBlockAsFound x)): board)
        let winningBoard = newMarkedBoards |> List.tryFind checkIfBoardIsWinner
        if winningBoard.IsSome then
          printfn $"We have a winner!! {winningBoard}"
          let winningBoardScore = calculateBoardScore winningBoard.Value
          printfn $"Winning score is : {winningBoardScore}"
          printfn $"Winning number was : {x}"
          printfn $"Final score: {winningBoardScore * x}"
          markBoardsP2 newMarkedBoards xs
        else markBoardsP2 newMarkedBoards xs

//    markBoardsP1 boards numberSequence
    markBoardsP2 boards numberSequence

