module Day2

  let sampleInput = [
     "2x3x4"
     "1x1x10"
  ]

  let input = TextFileReader.readFileContents "../../../Day2Input.txt"
  type dimensions = { length : int; width : int; height : int }

  let run =

     let mapToDimensions (s : string) =
        let dimensions = s.Split('x') |> Array.map int
        { length = dimensions.[0]; width = dimensions.[1]; height = dimensions.[2] }
     //part 1:
     input
     |> Seq.toList
     |> List.map mapToDimensions
     |> List.map (fun d ->
        let sides = [ d.length * d.width
                      d.width * d.height
                      d.height * d.length ]
        let smallestSide = sides |> List.min
        let surfaceArea = sides |> List.sum
        (surfaceArea * 2 + smallestSide)
        )
     |> List.sum
     |> printfn "Part 1: %d"

     input
     |> Seq.toList
     |> List.map mapToDimensions
     |> List.map (fun d ->
        let twoShortestSides = [ d.length; d.width; d.height ]
                               |> List.sort
                               |> List.take 2
        let ribbonLength =
           twoShortestSides
           |> List.sum
           |> (*) 2
        let bowLength = d.length * d.width * d.height
        (ribbonLength + bowLength)
        )
     |> List.sum
     |> printfn "Part 2: %d"
