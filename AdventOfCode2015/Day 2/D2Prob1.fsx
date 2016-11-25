let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__+ @"\D2input.txt");

let split (c : char) (s : string) =
    s.Split c |> Seq.toList

let gifts = input |> Array.map (fun (s : string) -> s |> split 'x' |> List.map System.Int32.Parse )

let sideSizeList (w : int) (l : int) (h : int) = [w*l;l*h;w*h]

let surface w l h = sideSizeList w l h |> List.map (fun x -> 2*x) |> List.sum

let smallestSideSize w l h = sideSizeList w l h |> List.min

let paper w l h = smallestSideSize w l h + surface w l h

gifts |> Array.map (fun g -> paper g.[0] g.[1] g.[2]) |> Array.sum |> printfn "%A" 