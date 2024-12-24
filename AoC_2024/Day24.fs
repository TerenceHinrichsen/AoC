module Day24

   open AdventOfCode
   open System
   open System.Text.RegularExpressions
   open System.Threading.Tasks

   let sampleInput = [
      "x00: 1"
      "x01: 0"
      "x02: 1"
      "x03: 1"
      "x04: 0"
      "y00: 1"
      "y01: 1"
      "y02: 1"
      "y03: 1"
      "y04: 1"
      ""
      "ntg XOR fgs -> mjb"
      "y02 OR x01 -> tnw"
      "kwq OR kpj -> z05"
      "x00 OR x03 -> fst"
      "tgd XOR rvg -> z01"
      "vdt OR tnw -> bfw"
      "bfw AND frj -> z10"
      "ffh OR nrd -> bqk"
      "y00 AND y03 -> djm"
      "y03 OR y00 -> psh"
      "bqk OR frj -> z08"
      "tnw OR fst -> frj"
      "gnj AND tgd -> z11"
      "bfw XOR mjb -> z00"
      "x03 OR x00 -> vdt"
      "gnj AND wpb -> z02"
      "x04 AND y00 -> kjc"
      "djm OR pbm -> qhw"
      "nrd AND vdt -> hwm"
      "kjc AND fst -> rvg"
      "y04 OR y02 -> fgs"
      "y01 AND x02 -> pbm"
      "ntg OR kjc -> kwq"
      "psh XOR fgs -> tgd"
      "qhw XOR tgd -> z09"
      "pbm OR djm -> kpj"
      "x03 XOR y03 -> ffh"
      "x00 XOR y04 -> ntg"
      "bfw OR bqk -> z06"
      "nrd XOR fgs -> wpb"
      "frj XOR qhw -> z04"
      "bqk OR frj -> z07"
      "y03 OR x01 -> nrd"
      "hwm AND bqk -> z03"
      "tgd XOR rvg -> z12"
      "tnw OR pbm -> gnj"
   ]

   let input =
      TextFileReader.readFileContents "../../../Day24Input.txt"

   type LogicGate  = | AND | OR | XOR

   type Gate = {
      input1 : string
      input2 : string
      output : string
      Logic   : LogicGate
   }

   let simulateGates (startingStates: Map<string, int>) (gates: List<Gate>) =
      let rec evaluate (state: Map<string, int>) (remainingGates: List<Gate>) =
         // Process one round of gates
         let updatedState, unresolvedGates =
            remainingGates |> List.fold (fun (currState, unresolved) gate ->
               let getValue (wire: string) = Map.tryFind wire currState // Look up in state

               let input1 = getValue gate.input1
               let input2 = getValue gate.input2

               match input1, input2 with
               | Some v1, Some v2 ->
                  let result =
                     match gate.Logic with
                     | AND -> if v1 = 1 && v2 = 1 then 1 else 0
                     | OR -> if v1 = 1 || v2 = 1 then 1 else 0
                     | XOR -> if v1 <> v2 then 1 else 0

                  let newState = Map.add gate.output result currState
                  (newState, unresolved) // Gate resolved, don't add to unresolved
               | _ -> (currState, gate :: unresolved) // Inputs not ready, keep unresolved
            ) (state, [])

         // If there are unresolved gates, recursively process them
         if unresolvedGates = [] then updatedState
         else evaluate updatedState unresolvedGates

      evaluate startingStates gates


   let run =
      let startingStates =
         input
         |> List.takeWhile (fun x -> x <> "")
         |> List.map (fun x ->
            let parts = x.Split(": ")
            parts[0], int parts[1]
         )
         |> Map.ofList

      let gates =
         input
         |> List.skipWhile (fun x -> x <> "")
         |> List.skip 1
         |> List.map (fun x ->
            let parts = x.Split(" ")
            let input1 = parts[0]
            let input2 = parts[2]
            let output = parts[4]
            let logic =
               match parts[1] with
               | "AND" -> AND
               | "OR" -> OR
               | "XOR" -> XOR
               | _ -> failwith "Unknown logic gate"
            { input1 = input1; input2 = input2; output = output; Logic = logic }
         )

      let finalState = simulateGates startingStates gates

      let zWires =
         finalState
         |> Map.filter (fun key _ -> key.StartsWith("z"))
         |> Map.toList
         |> List.sortBy fst
         |> List.map snd

      let binaryNumber =
         String.Join("", zWires |> List.rev |> List.map string)
      printfn $"Binary number: {binaryNumber}"
      let decimalValue = Convert.ToInt64(binaryNumber, 2)
      decimalValue

   printfn $"Output: {run}"
