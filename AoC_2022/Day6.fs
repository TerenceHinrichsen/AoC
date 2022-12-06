namespace AdventOfCode
open Microsoft.FSharp.Core

module Day6 =

   let sampleInput = [
      "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
   ]

   let input = TextFileReader.readFileContents "../../../Day6Input.txt" |> Seq.toList
   module Part1 =
      let run =

         let charL =
            input
            |> List.head
            |> Seq.toList

         // last four char's are unique

         charL
         |> List.mapi (fun i x ->
            if i > 3 then
               (charL[i-3], charL[i-2], charL[i-1], charL[i]), i
            else (x,x,x,x), i
         )
         |> List.filter (fun ((c1, c2, c3, c4), i) ->
            c1 <> c2 &&
            c1 <> c3 &&
            c1 <> c4 &&
            c2 <> c3 &&
            c2 <> c4 &&
            c3 <> c4
            )
         |> List.head
         |> fun ((c1,c2,c3,c4),i) ->
            printfn $"Characters {c1}, {c2}, {c3}, {c4} at {i + 1}"



   module Part2 =
      let run =

         let charL =
            input
            |> List.head
            |> Seq.toList

         let distinctWindow =
            charL
            |> List.windowed 14
            |> List.map ( fun x -> x |> List.distinct)
            |> List.filter (fun x -> x.Length = 14)
            |> List.head
            |> Array.ofList
            |> System.String.Concat

         let completeS =
            input
            |> List.head

         printfn $"Position : {completeS.IndexOf(distinctWindow) + 14}"


   let run =
      Part1.run
      Part2.run