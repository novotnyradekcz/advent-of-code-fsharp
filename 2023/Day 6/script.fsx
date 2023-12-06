let lines = System.IO.File.ReadAllLines("2023\Day 6\input.txt")
let testLines = [|
        "Time:      7  15   30"
        "Distance:  9  40  200"
    |]

let one (lines: string[]) =
    (1,
     (lines[0].Split()[1..] |> Array.filter (fun x -> x <> "") |> Array.map int,
      lines[1].Split()[1..] |> Array.filter (fun x -> x <> "") |> Array.map int)
     ||> Array.zip
     |> Array.map (fun race ->
         Array.init (fst race) (fun i -> i * (fst race - i))
         |> Array.filter (fun dist -> dist > snd race)
         |> Array.length))
    ||> Array.fold (fun product ways -> product * ways)

let two (lines: string[]) =
    let race = (("", lines[0].Split()[1..]) ||> Array.fold (fun a b -> a + b) |> int64,
                ("", lines[1].Split()[1..]) ||> Array.fold (fun a b -> a + b) |> int64)
    Array.init (race |> fst |> int) (fun i -> int64 i * (fst race - int64 i))
    |> Array.filter (fun dist -> dist > snd race)
    |> Array.length


printfn $"Part one test: %d{one testLines}"
printfn $"Part one answer: %d{one lines}"
printfn $"Part two test: %d{two testLines}"
printfn $"Part two answer: %d{two lines}"