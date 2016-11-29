let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__+ @"\D19input.txt");

let (|UpperCase|_|) c =
    match c with
    | a when a >= 'A' && a <= 'Z' -> Some(c)
    | _ -> None

let (|LowerCase|_|) c =
    match c with
    | a when a >= 'a' && a <= 'z' -> Some(c)
    | _ -> None

let parseMolecule (str : string) =
    let rec parseMolecule' charList out =
        match charList with
        | [] -> out
        | UpperCase a::LowerCase b::tail -> parseMolecule' tail ((new string [|a;b|])::out)
        | UpperCase a::tail -> parseMolecule' tail ((new string [|a|])::out)
        | 'e'::tail -> parseMolecule' tail ("e"::out)
        | _ -> failwith ("could not parse molecule: "+str)
    parseMolecule' (Seq.toList str) [] |> List.rev

let start = input |> Seq.last |> parseMolecule

let symbolCounts = start |> Seq.countBy id |> Map.ofSeq

Seq.length start - symbolCounts.["Rn"] - symbolCounts.["Ar"] - 2*symbolCounts.["Y"] - 1 |> printfn "%A"