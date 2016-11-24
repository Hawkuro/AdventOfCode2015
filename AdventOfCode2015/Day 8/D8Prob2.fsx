let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__+ @"\D8input.txt");

let calcStringDiff str = 
    let calcStringDiff' c =
        match c with
        | '\"' | '\\' -> 2
        | _ -> 1
    str |> Seq.map calcStringDiff' |> Seq.sum |> (+) 2 |> (fun i -> i - (Seq.length str))

input |> Seq.map calcStringDiff |> Seq.sum |> printfn "%A"