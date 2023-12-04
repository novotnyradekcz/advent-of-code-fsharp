let lines = System.IO.File.ReadAllLines("2023\Day 2\input.txt")
let testLines = [|
        "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
        "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
        "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
        "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
        "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
    |]

let one lines =
    lines
    |> Array.map (fun (line: string) ->
        line.Split(':', ';', ',')
        |> Array.map (fun section -> section.Split(' '))
        |> Array.map (fun s -> s |> Array.filter (fun x -> x <> ""))
        |> Array.map (fun section ->
            match section[1] with
            | "red" -> if int section[0] <= 12 then 0 else 1
            | "green" -> if int section[0] <= 13 then 0 else 1
            | "blue" -> if int section[0] <= 14 then 0 else 1
            | _ -> 0)
        |> Array.sum)
    |> Array.mapi (fun i x ->
        if x = 0 then i + 1 else 0)
    |> Array.sum

let two lines =
    lines
    |> Array.map (fun (line: string) ->
        line.Split(':', ';', ',')
        |> Array.map (fun section -> section.Split(' '))
        |> Array.map (fun s -> s |> Array.filter (fun x -> x <> ""))
        |> Array.map (fun section ->
            match section[1] with
            | "red" -> int section[0], 0, 0
            | "green" -> 0, int section[0], 0
            | "blue" -> 0, 0, int section[0]
            | _ -> 0, 0, 0)
        |> Array.unzip3
        |||> (fun x y z -> (x |> Array.max) * (y |> Array.max) * (z |> Array.max)))
    |> Array.sum

printfn $"Part one test: %d{one testLines}"
printfn $"Part one answer: %d{one lines}"
printfn $"Part two test: %d{two testLines}"
printfn $"Part two answer: %d{two lines}"