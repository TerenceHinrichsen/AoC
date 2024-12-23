module Day23

   open AdventOfCode
   open System
   open System.Text.RegularExpressions
   open System.Threading.Tasks

   let sampleInput = [
      "kh-tc"
      "qp-kh"
      "de-cg"
      "ka-co"
      "yn-aq"
      "qp-ub"
      "cg-tb"
      "vc-aq"
      "tb-ka"
      "wh-tc"
      "yn-cg"
      "kh-ub"
      "ta-co"
      "de-co"
      "tc-td"
      "tb-wq"
      "wh-td"
      "ta-ka"
      "td-qp"
      "aq-cg"
      "wq-ub"
      "ub-vc"
      "de-ta"
      "wq-aq"
      "wq-vc"
      "wh-yn"
      "ka-de"
      "kh-ta"
      "co-tc"
      "wh-qp"
      "tb-vc"
      "td-yn"
   ]

   let input =
      TextFileReader.readFileContents "../../../Day23Input.txt"

   let run =

      let connections =
         input
         |> List.map (fun line ->
            let p1 = Array.get (line.Split('-')) 0
            let p2 = Array.get (line.Split('-')) 1
            (p1,p2)
            )
         |> Set.ofList

      let uniqueDevices =
         connections
         |> Set.map (fun (a,b) -> [a;b])
         |> List.concat
         |> List.distinct

      let networkMap =
         // for each unique device, find all the connections for that device
         uniqueDevices
         |> List.map (fun device ->
            let connectionsForDevice =
               connections
               |> Set.filter (fun (a,b) ->
                  a = device || b = device)
               |> Set.map (fun (a,b) -> if a = device then b else a)
            device, connectionsForDevice
            )
         |> Map.ofList

      // find all devices which are interconnected. so for each devices, find it in the network map and then check whether
      // the device is in the list of connected devices for that device.


      // network map is correct, so now we need to find a network of three.
      // so for each host, we know which devices it is connected to.
      // so we need to check that for each device, it is connected to the other two devices.
      // so we can filter out all devices which do not have a connection to the other devices in
      // our current hosts network.

      let groupsOfThree (networkMap: Map<string, Set<string>>) =
         [ for c1 in networkMap.Keys do
            for c2 in networkMap.Keys -> c1, c2 ]
         |> List.filter (fun (c1, c2) -> Set.contains c1 networkMap[c2] && Set.contains c2 networkMap[c1])
         |> List.map (fun (c1, c2) -> c1, c2, Set.intersect networkMap[c1] networkMap[c2])
         |> List.map (fun (c1, c2, intersection) -> intersection |> Set.map (fun c3 -> Set.ofList [ c1; c2; c3 ]))
         |> Set.unionMany

      groupsOfThree networkMap
      |> Set.filter (fun set -> Set.exists (fun (c: string) -> c[0] = 't') set)
      |> Set.count
      |> fun x -> printfn $"Part 1: {x}"

      ()
