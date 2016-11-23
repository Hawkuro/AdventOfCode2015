let input = System.IO.File.ReadAllLines(@"C:\Users\hawku\Documents\Visual Studio 2015\Projects\AdventOfCode2015\AdventOfCode2015\Day 8\D8input.txt");

let calcStringDiff str = 
    let rec calcStringDiff' str n =
        match str with
        | [] -> n
        | '\\'::'x'::_::_::tail -> calcStringDiff' tail (n+3)
        | '\\'::_::tail -> calcStringDiff' tail (n+1)
        | _::tail -> calcStringDiff' tail n
    calcStringDiff' (Seq.toList str) 2

input |> Seq.map calcStringDiff |> Seq.sum |> printfn "%A"