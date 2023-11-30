module Day3

  let sampleInput = [
     "^v^v^v^v^v"
  ]

  let input = TextFileReader.readFileContents "../../../Day3Input.txt"

  type Direction = | North | South | East | West
  type Position = { Lat: int; Long: int}

  let run =

     let mapToDirection (c: char) =
        match c with
        | '^' -> North
        | 'v' -> South
        | '>' -> East
        | '<' -> West
        | _ -> failwith "invalid direction"

     let getFirstRow (input: seq<string>) =
        input |> Seq.toList |> List.head |> Seq.toList
     let startingPosition = {Lat = 0; Long = 0}

     let getDestination (state: List<Position>) direction =
        let lastPosition = List.last state
        let nextPosition =
           match direction with
           | North -> { Lat = lastPosition.Lat + 1; Long = lastPosition.Long }
           | South -> { Lat = lastPosition.Lat - 1; Long = lastPosition.Long }
           | East ->  { Lat = lastPosition.Lat; Long = lastPosition.Long + 1 }
           | West ->  { Lat = lastPosition.Lat; Long = lastPosition.Long - 1 }
        List.append state [nextPosition]


     //part 1:
     input
     |> getFirstRow
     |> List.map mapToDirection
     |> List.fold getDestination [startingPosition]
     |> List.groupBy id
     |> List.map (fun (k, v) -> k, v.Length)
     |> List.filter (fun (k, v) -> v > 0)
     |> List.length
     |> printfn "part 1: %d"



     |> id
