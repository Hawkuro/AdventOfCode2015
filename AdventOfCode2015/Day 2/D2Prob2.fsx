let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__+ @"\D2input.txt");

let split (c : char) (s : string) =
    s.Split c |> Seq.toList

let gifts = input |> Array.map (fun (s : string) -> s |> split 'x' |> List.map System.Int32.Parse )


let minPerimeter w l h = w + l + h - List.max [w;l;h] |> (*) 2

let volume w l h = w*l*h

let ribbon w l h = minPerimeter w l h + volume w l h


gifts |> Array.map (fun g -> ribbon g.[0] g.[1] g.[2]) |> Array.sum |> printfn "%A" 