let lines = System.IO.File.ReadAllLines("2023\Day 5\input.txt")
let testLines = [|
        "seeds: 79 14 55 13"
        ""
        "seed-to-soil map:"
        "50 98 2"
        "52 50 48"
        ""
        "soil-to-fertilizer map:"
        "0 15 37"
        "37 52 2"
        "39 0 15"
        ""
        "fertilizer-to-water map:"
        "49 53 8"
        "0 11 42"
        "42 0 7"
        "57 7 4"
        ""
        "water-to-light map:"
        "88 18 7"
        "18 25 70"
        ""
        "light-to-temperature map:"
        "45 77 23"
        "81 45 19"
        "68 64 13"
        ""
        "temperature-to-humidity map:"
        "0 69 1"
        "1 0 69"
        ""
        "humidity-to-location map:"
        "60 56 37"
        "56 93 4"
    |]

let parseInput (lines: string[]) =
    let seeds =
        lines[0].Split() |> Array.skip 1 |> Array.map int64
    let createMapper mapperName =
        lines
        |> Array.skip ((lines |> Array.findIndex (fun line -> line = mapperName)) + 1)
        |> Array.takeWhile (fun line -> line <> "")
        |> Array.map (fun line -> line.Split() |> Array.map int64)
    let mappers =
        [|
            "seed-to-soil map:"
            "soil-to-fertilizer map:"
            "fertilizer-to-water map:"
            "water-to-light map:"
            "light-to-temperature map:"
            "temperature-to-humidity map:"
            "humidity-to-location map:"
        |]
        |> Array.map createMapper
    
    seeds, mappers

let mapForward (mappers: int64 array array array) (seed: int64) =
    (seed, mappers)
    ||> Array.fold (fun input mapping ->
        let mapsTo =
            mapping
            |> Array.filter (fun interval -> input >= interval[1] && input < interval[1] + interval[2])
        match mapsTo with
        | [| result |] -> result[0] + input - result[1]
        | _ -> input)

let mapBackward (mappers: int64 array array array) (location: int64) =
    (mappers, location)
    ||> Array.foldBack (fun mapping input ->
        let mapsTo =
            mapping
            |> Array.filter (fun interval -> input >= interval[0] && input < interval[0] + interval[2])
        match mapsTo with
        | [| result |] -> result[1] + input - result[0]
        | _ -> input)

let one (lines: string[]) =
    let seeds, mappers = parseInput lines

    seeds
    |> Array.map (mapForward mappers)
    |> Array.min

let two (lines: string[]) =
    let seeds, mappers = parseInput lines

    Array.init 100000000 int64  // if the answer is wrong, increase this (will run longer)
    |> Array.map (mapBackward mappers)
    |> Array.filter (fun seed ->
        Array.init (seeds.Length / 2) (fun x -> 2 * x)
        |> Array.map (fun i -> seed >= seeds[i] && seed < seeds[i] + seeds[i + 1])
        |> Array.exists (fun x -> x = true))
    |> Array.tryHead
    |> Option.map (mapForward mappers) 
    |> Option.defaultValue 42


printfn $"Part one test: %d{one testLines}"
printfn $"Part one answer: %d{one lines}"
printfn $"Part two test: %d{two testLines}"
printfn $"Part two answer: %d{two lines}"