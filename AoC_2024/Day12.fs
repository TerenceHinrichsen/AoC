module Day12

   open AdventOfCode
   open System
   open System.Text.RegularExpressions
   open System.Threading.Tasks

   let sampleInput = seq [
      "RRRRIICCFF"
      "RRRRIICCCF"
      "VVRRRCCFFF"
      "VVRCCCJFFF"
      "VVVVCJJCFE"
      "VVIVCCJJEE"
      "VVIIICJJEE"
      "MIIIIIJJEE"
      "MIIISIJEEE"
      "MMMISSJEEE"
   ]

   let input =
      TextFileReader.readFileContents "../../../Day12Input.txt"

   let run = ()
