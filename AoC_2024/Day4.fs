module Day4

   open AdventOfCode
   open System
   open System.Text.RegularExpressions

   let sampleInput = [
      "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
   ]

   let input =
      TextFileReader.readFileContents "../../../Day4Input.txt"


   let run = ()
