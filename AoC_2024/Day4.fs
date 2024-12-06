module Day4

   open AdventOfCode
   open System
   open System.Text.RegularExpressions
   open FSharpPlus

   let sampleInput = [
      "MMMSXXMASM"
      "MSAMXMSMSA"
      "AMXSXMAAMM"
      "MSAMASMSMX"
      "XMASAMXAMM"
      "XXAMMXXAMA"
      "SMSMSASXSS"
      "SAXAMASAAA"
      "MAMMMXMMMM"
      "MXMXAXMASX"
   ]

   let input =
      TextFileReader.readFileContents "../../../Day4Input.txt"


   type Direction =
      | Horizontal
      | Vertical
      | Diagonal

   let run =
      let rotate90 m =
         let h, w = Array2D.length1 m, Array2D.length2 m
         Array2D.init w h (fun r c -> m[h - c - 1, r])

      let rows m =
         let h, w = Array2D.length1 m, Array2D.length2 m

         [ for k in 0..h - 1 -> [ for j in 0..w - 1 -> m[k, j] ] ]

      let diagonals m =
         let h, w = Array2D.length1 m, Array2D.length2 m

         [ for k in 0 .. (h + w - 2) ->
               [ for j in 0..k do
                     if (k - j < h) && (j < w) then
                        yield m[k - j, j] ] ]

      let countXmasInLine l =
         let regex = Regex("XMAS", RegexOptions.Compiled)
         String.ofSeq l |> regex.Matches |> _.Count

      let rec countAllXmas grid =
         let rec loop rotation grid =
            if rotation = 360 then
               0
            else
               let rowCount = grid |> rows |> List.sumBy countXmasInLine
               let diagCount = grid |> diagonals |> List.sumBy countXmasInLine
               rowCount + diagCount + loop (rotation + 90) (rotate90 grid)

         loop 0 grid

      let aCrosses m =
         let h, w = Array2D.length1 m, Array2D.length2 m

         seq {
            for k in 1 .. h - 2 do
               yield!
                  seq {
                     for j in 1 .. w - 2 do
                        if m[k, j] = 'A' then
                           yield
                              (String.ofList [ m[k - 1, j - 1]; 'A'; m[k + 1, j + 1] ],
                                 String.ofList [ m[k + 1, j - 1]; 'A'; m[k - 1, j + 1] ])
                  }
         }

      let countAllCrossMas grid =
         grid |> aCrosses |> Seq.filter (fun (l, r) -> (l = "SAM" || l = "MAS") && (r = "SAM" || r = "MAS")) |> Seq.length

      let input = (Seq.map String.toList >> Seq.toList >> array2D) input

      printfn $"Day 4 - Part 1: {countAllXmas input}"
      printfn $"Day 4 - Part 2: {countAllCrossMas input}"
