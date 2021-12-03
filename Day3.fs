module Day3

  let sampleInput = [
    "00100"
    "11110"
    "10110"
    "10111"
    "10101"
    "01111"
    "00111"
    "11100"
    "10000"
    "11001"
    "00010"
    "01010" ]

  let input = TextFileReader.readFileContents "../../../Day3Input.txt"

  let parseInput (input: seq<string>) =
    input
    |> Seq.map (fun row -> row |> Seq.toList)
    |> Seq.toList


  let run =

    let list =
      input
      |> parseInput
      |> List.map (fun row ->
        row
        |> Seq.toList
        |> List.mapi (fun i c -> i , c, (if c = '0' then 1 else 0) , (if c = '1' then 1 else 0)) )
      |> List.concat

    let gammaRate, epsilonRate =
        list
        |> List.groupBy (fun (index,_,_,_) -> index)
        |> List.map (fun (index, listOfInputs) ->
          let numberOfZeros = listOfInputs |> List.sumBy (fun (_,_,a,b) -> a)
          let numberOfOnes = listOfInputs |> List.sumBy (fun (_,_,_,b) -> b)
          if numberOfZeros > numberOfOnes then '0' else '1'
          , if numberOfZeros < numberOfOnes then '0' else '1' )
        |> List.unzip

    let gammaRate = gammaRate |> List.map string |> Seq.ofList |> String.concat ""
    let epsilonRate = epsilonRate |> List.map string |> Seq.ofList |> String.concat ""
    printfn $"Gamma result {System.Convert.ToInt32(gammaRate, 2)}"
    printfn $"Gamma result {System.Convert.ToInt32(epsilonRate, 2)}"

    printfn $"Answer is {System.Convert.ToInt32(gammaRate, 2) * System.Convert.ToInt32(epsilonRate, 2)}"

    let list = (input |> List.ofSeq)

    let findBitInString (inputL : List<string>) bitNumber =
      inputL
      |> List.map (fun st -> st.[bitNumber])
      |> List.groupBy id
      |> List.map (fun (char, charL) -> char, charL.Length)
      |> List.sortBy fst
      |> List.pairwise
      |> List.head

    let oxygenRating =
      let rec filterDownL bitNumber (inputL : List<string>) =
        match inputL with
        | [x] -> x
        | x ->
          printfn $"Calculating bit {bitNumber}"
          let zeros, ones = findBitInString x bitNumber
          if ones |> snd >= (zeros |> snd) then
            printfn $"Ones was greater {ones |> snd} vs {zeros |> snd }"
            let newList =
              x |> List.filter (fun y -> y.[bitNumber] = '1')
            printfn $"New list is {newList} and contains {newList.Length} elements"
            filterDownL (bitNumber + 1) newList
          else
            printfn $"Zeros was greater {ones |> snd} vs {zeros |> snd }"
            let newList =
              x |> List.filter (fun y -> y.[bitNumber] = '0')
            printfn $"New list is {newList} and contains {newList.Length} elements"
            filterDownL (bitNumber + 1) newList

      filterDownL 0 list

    printfn $"-------------- Oxygen rating is {oxygenRating} --------------------------"

    let co2Rating =
      let rec filterDownL bitNumber (inputL : List<string>) =
        match inputL with
        | [x] -> x
        | x ->
          printfn $"Calculating bit {bitNumber}"
          let zeros, ones = findBitInString x bitNumber
          if  (zeros |> snd) <= (ones |> snd) then
            printfn $"Zeros was fewer {ones |> snd} vs {zeros |> snd }"
            let newList =
              x |> List.filter (fun y -> y.[bitNumber] = '0')
            printfn $"New list is {newList} and contains {newList.Length} elements"
            filterDownL (bitNumber + 1) newList
          else
            printfn $"Ones was fewer {ones |> snd} vs {zeros |> snd }"
            let newList =
              x |> List.filter (fun y -> y.[bitNumber] = '1')
            printfn $"New list is {newList} and contains {newList.Length} elements"
            filterDownL (bitNumber + 1) newList

      filterDownL 0 list

    printfn $"-------------- Oxygen rating is {co2Rating} --------------------------"
    printfn $"Answer is {System.Convert.ToInt32(oxygenRating, 2) * System.Convert.ToInt32(co2Rating, 2)}"