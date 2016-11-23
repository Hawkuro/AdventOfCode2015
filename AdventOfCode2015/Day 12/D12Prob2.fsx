#r @"C:\Users\hawku\Documents\Visual Studio 2015\Projects\AdventOfCode2015\packages\FSharp.Data.2.3.2\lib\net40\FSharp.Data.dll"
open FSharp.Data

let data = JsonValue.Load(@"C:\Users\hawku\Documents\Visual Studio 2015\Projects\AdventOfCode2015\AdventOfCode2015\Day 12\D12input.txt")

let JsonIsRed json =
    match json with
    | JsonValue.String s when s="red" -> true
    | _ -> false

let rec getJSONTotal json = 
    match json with 
    | JsonValue.Number i -> i
    | JsonValue.Record r when r |> Seq.exists (fun (n,d) -> JsonIsRed d) -> 0m
    | JsonValue.Record r -> r |> Seq.map (fun (n,d) -> getJSONTotal d) |> Seq.sum 
    | JsonValue.Array a -> a |> Seq.map getJSONTotal |> Seq.sum
    | JsonValue.String s -> 0m
    | JsonValue.Float f -> 0m
    | JsonValue.Boolean b -> 0m
    | JsonValue.Null -> 0m


getJSONTotal data |> printfn "%A"