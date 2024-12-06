module Day5

   open AdventOfCode
   open System
   open System.Text.RegularExpressions
   open FSharpPlus

   let sampleInput = [
      "47|53"
      "97|13"
      "97|61"
      "97|47"
      "75|29"
      "61|13"
      "75|53"
      "29|13"
      "97|29"
      "53|29"
      "61|53"
      "97|53"
      "61|29"
      "47|13"
      "75|47"
      "97|75"
      "47|61"
      "75|61"
      "47|29"
      "75|13"
      "53|13"
      ""
      "75,47,61,53,29"
      "97,61,53,29,13"
      "75,29,13"
      "75,97,47,61,53"
      "61,13,29"
      "97,13,75,29,47"
   ]

   let input =
      TextFileReader.readFileContents "../../../Day5Input.txt"

   let run =
      let part1, part2 =
         sampleInput
         |> List.splitOnceOnCharExcl (fun x -> x = "")

      let pageRules =
         part1
         |> List.map (fun x -> x.Split('|'))
         |> List.map (fun x -> (int x[0], int x[1]))

      let pages =
         part2
         |> List.map (fun x -> x.Split(','))
         |> List.map (fun x -> x |> Array.map int)

      // so for each value we need to check whether it is in the correct order
      // based on the rules, so first value, check that all remaining values
      // are allowed according to the page rules
      // now the second value, check that all remaining values are allowed and that
      let isSectionValid (section: List<int>) (rules: List<int * int>) : bool =
         section
         |> List.pairwise
         |> List.map (fun (before, after) ->
            // check the BEFORE value is allowed (not the other way around).
            let allowedValues = rules |> List.filter (fun (a, b) -> b = before)
            ()

            )
         true


      // now to check that the pages are in correct order based on the rules
      let correctlyOrderedLists =
         pages
         |> List.mapi (fun index section ->
            printfn $"Checking section {index}"
            section
            |> Array.toList
            |> List.filter (fun x ->
               let applicableRules = pageRules |> List.filter (fun (a, b) -> a = x )
               let isValid = isSectionValid (section |> Array.toList) applicableRules
               printfn $"Checking {x} - {isValid}"
               isValid
               )
            )

      printfn $"xx = {pageRules}"
