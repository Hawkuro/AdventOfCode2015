let input = System.IO.File.ReadAllLines(@"C:\Users\hawku\Documents\Visual Studio 2015\Projects\AdventOfCode2015\AdventOfCode2015\Day 8\D8input.txt");

let calcStringDiff str = 
    let calcStringDiff' c =
        match c with
        | '\"' | '\\' -> 2
        | _ -> 1
    str |> Seq.map calcStringDiff' |> Seq.sum |> (+) 2 |> (-) (Seq.length str)

input |> Seq.map calcStringDiff |> Seq.sum |> printfn "%A"