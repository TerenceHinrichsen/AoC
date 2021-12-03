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
        |> List.mapi (fun i c -> i , (if c = '0' then 1 else 0) , (if c = '1' then 1 else 0)) )
      |> List.concat

    let gammaRate, epsilonRate =
        list
        |> List.groupBy (fun (index,_,_) -> index)
        |> List.map (fun (index, listOfInputs) ->
          let numberOfZeros = listOfInputs |> List.sumBy (fun (_,a,b) -> a)
          let numberOfOnes = listOfInputs |> List.sumBy (fun (_,_,b) -> b)
          if numberOfZeros > numberOfOnes then '0' else '1'
          , if numberOfZeros < numberOfOnes then '0' else '1' )
        |> List.unzip

    let gammaRate = gammaRate |> List.map string |> Seq.ofList |> String.concat ""
    let epsilonRate = epsilonRate |> List.map string |> Seq.ofList |> String.concat ""
    printfn $"Gamma result {System.Convert.ToInt32(gammaRate, 2)}"
    printfn $"Gamma result {System.Convert.ToInt32(epsilonRate, 2)}"

    printfn $"Answer is {System.Convert.ToInt32(gammaRate, 2) * System.Convert.ToInt32(epsilonRate, 2)}"
    ()