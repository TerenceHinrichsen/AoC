module Day7

   open AdventOfCode
   open System
   open System.Text.RegularExpressions
   open FsToolkit.ErrorHandling

   let sampleInput = [
      "32T3K 765"
      "T55J5 684"
      "KK677 28 "
      "KTJJT 220"
      "QQQJA 483"
      ]
   let input =
      TextFileReader.readFileContents "../../../Day7Input.txt"

   let cardL = [
      'A'
      'K'
      'Q'
      'J'
      'T'
      '9'
      '8'
      '7'
      '6'
      '5'
      '4'
      '3'
      '2'
   ]
   let cardLRound2 = [
      'A'
      'K'
      'Q'
      'T'
      '9'
      '8'
      '7'
      '6'
      '5'
      '4'
      '3'
      '2'
      'J'
   ]
   type CardHand =
      | FiveOfKind
      | FourOfKind
      | FullHouse
      | ThreeOfKind
      | TwoPair
      | OnePair
      | HighCard

   type Hand = {
      cards: List<char>
      bid: int
   }

   let parseToHand (cardsAndBid: List<string>) =
      let hand =
         cardsAndBid
         |> List.head
         |> Seq.toList
      let bid =
         cardsAndBid
         |> List.tail
         |> List.map (fun x -> x.Trim() |> int)
         |> List.head
      {
         cards = hand
         bid = bid
      }

   let determineHand countOfCards =
      match countOfCards with
      | [(_, 5)] -> FiveOfKind
      | [(_, 4); (_, 1)] -> FourOfKind
      | [(_, 3); (_, 2)] -> FullHouse
      | [(_, 3); (_, 1); (_, 1)] -> ThreeOfKind
      | [(_, 2); (_, 2); (_, 1)] -> TwoPair
      | [(_, 2); (_, 1); (_, 1); (_, 1)] -> OnePair
      | _ -> HighCard

   let calculateHandStrength (hand: Hand) =
      hand.cards
      |> List.groupBy id
      |> List.map (fun (card, group) -> (card, List.length group))
      |> List.sortBy snd
      |> List.rev
      |> determineHand

   // now the 'J' is a wildcard and is added to the strongest group.
   let calculateHandStrengthRound2 (hand: Hand) =
      hand.cards
      |> List.groupBy id
      |> List.map (fun (card, group) -> (card, List.length group))
      |> List.sortBy snd
      |> List.rev
      |> List.map (fun (card, count) ->
         if card = 'J' then (card, count + 1) else (card, count))
      |> determineHand


   let parseInputToAnswer (input: List<string>) =
      // orderHandsInOrderOfRank
      // Add position
      // Multiply bid by rank.
      // Add up all the bids.

      input
      |> List.map (fun x -> x.Split(' ') |> Array.toList |> List.filter (fun x -> x <> ""))
      |> List.map parseToHand
      |> List.map (fun x -> (x, calculateHandStrength x))
      |> List.sortBy (fun (hand, strength) ->
         strength
         , List.findIndex (fun x -> x = hand.cards[0]) cardL
         , List.findIndex (fun x -> x = hand.cards[1]) cardL
         , List.findIndex (fun x -> x = hand.cards[2]) cardL
         , List.findIndex (fun x -> x = hand.cards[3]) cardL
         , List.findIndex (fun x -> x = hand.cards[4]) cardL
         )
      |> List.rev
      |> List.mapi (fun index (hand, strength) ->
         printfn $"Position {index + 1} - {hand.cards |> List.toArray |> String} - {strength} - {hand.bid}"
         hand.bid * (index + 1)
         )
      |> List.sum
      |> fun x -> printfn $"Part1 : {x}"

      input
      |> List.map (fun x -> x.Split(' ') |> Array.toList |> List.filter (fun x -> x <> ""))
      |> List.map parseToHand
      |> List.map (fun x -> (x, calculateHandStrengthRound2 x))
      |> List.sortBy (fun (hand, strength) ->
         strength
         , List.findIndex (fun x -> x = hand.cards[0]) cardL
         , List.findIndex (fun x -> x = hand.cards[1]) cardL
         , List.findIndex (fun x -> x = hand.cards[2]) cardL
         , List.findIndex (fun x -> x = hand.cards[3]) cardL
         , List.findIndex (fun x -> x = hand.cards[4]) cardL
         )
      |> List.rev
      |> List.mapi (fun index (hand, strength) ->
         printfn $"Position {index + 1} - {hand.cards |> List.toArray |> String} - {strength} - {hand.bid}"
         hand.bid * (index + 1)
         )
      |> List.sum
      |> fun x -> printfn $"Part2 : {x}"

   let run =
      parseInputToAnswer sampleInput
