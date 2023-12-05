let lines = System.IO.File.ReadAllLines("2023\Day 4\input.txt")
let testLines = [|
        "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
        "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
        "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
        "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
        "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
        "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
    |]

let wins (lines: string[]) winningNums =
    lines
    |> Array.map (fun line ->
        line.Split()
        |> Array.filter (fun x -> x <> ""))
    |> Array.map (fun card ->
        card[2..winningNums + 2]
        |> Array.map (fun winningNum ->
            if card[winningNums + 3..] |> Array.contains winningNum then 1 else 0)
        |> Array.sum)

let one (lines: string[]) winningNums =
    wins lines winningNums
    |> Array.filter (fun x -> x <> 0)
    |> Array.map (fun wins ->
        2.0 ** (float wins - 1.0))
    |> Array.sum |> int

let two (lines: string[]) winningNums =
    let wins = wins lines winningNums
    let cards = Array.create lines.Length 1
    for i in 0..lines.Length - 1 do
        for j in 0..wins[i] - 1 do
            if i + j + 1 < lines.Length then
                cards[i + j + 1] <- cards[i + j + 1] + cards[i]
    cards |> Array.sum


printfn $"Part one test: %d{one testLines 5}"
printfn $"Part one answer: %d{one lines 10}"
printfn $"Part two test: %d{two testLines 5}"
printfn $"Part two answer: %d{two lines 10}"