let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__+ @"\D1input.txt");

let upOrDown n c =
    match c with
    | '(' -> n+1
    | ')' -> n-1
    | _ -> n

input |> Seq.toList |> List.fold upOrDown 0 |> printfn "%d"