let lines = System.IO.File.ReadAllLines("2023\Day 1\input.txt")
let testLinesOne = [|
        "1abc2"
        "pqr3stu8vwx"
        "a1b2c3d4e5f"
        "treb7uchet"
    |]

let testLinesTwo = [|
        "two1nine"
        "eightwothree"
        "abcone2threexyz"
        "xtwone3four"
        "4nineeightseven2"
        "zoneight234"
        "7pqrstsixteen"
    |]

let one lines =
    lines
    |> Array.map (fun line ->
        (line |> Seq.find (fun x -> x |> System.Char.IsDigit),
         line |> Seq.findBack (fun x -> x |> System.Char.IsDigit))
        ||> (fun x y -> string x + string y |> int))
    |> Array.sum 


let numbers = [ "zero"; "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" ]

let findFirstDigit (line: string) =
    let words =
        numbers
        |> List.map (fun num -> num |> line.IndexOf)
        |> List.filter (fun x -> x >= 0)
    let symbol =
        line
        |> Seq.tryFindIndex (fun x -> x |> System.Char.IsDigit)
        |> Option.defaultValue line.Length
    match words with
    | [] -> line[symbol] |> string |> int
    | ws ->
        let i = ws |> List.min
        if i < symbol then
            numbers
            |> List.findIndex (fun x -> x |> line[i..].StartsWith)
        else
            line[symbol] |> string |> int

let findLastDigit (line: string) =
    let words =
        numbers
        |> List.map (fun num -> num |> line.LastIndexOf)
        |> List.filter (fun x -> x >= 0)
    let symbol =
        line
        |> Seq.tryFindIndexBack (fun x -> x |> System.Char.IsDigit)
        |> Option.defaultValue 0
    match words with
    | [] -> line[symbol] |> string |> int
    | ws ->
        let i = ws |> List.max
        if i > symbol then
            numbers
            |> List.findIndex (fun x -> x |> line[i..].StartsWith)
        else
            line[symbol] |> string |> int

let two lines =
    lines
    |> Array.map (fun line ->
        (line |> findFirstDigit,
         line |> findLastDigit)
        ||> (fun x y -> string x + string y |> int))
    |> Array.sum

printfn $"Part one test: %d{one testLinesOne}"
printfn $"Part one answer: %d{one lines}"
printfn $"Part two test: %d{two testLinesTwo}"
printfn $"Part two answer: %d{two lines}"