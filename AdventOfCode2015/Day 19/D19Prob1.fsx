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
        | LowerCase a::tail -> parseMolecule' tail ((new string [|a|])::out)
        | _ -> failwith ("could not parse molecule: "+str)
    parseMolecule' (Seq.toList str) [] |> List.rev

let start = input |> Seq.last |> parseMolecule

let parseTransform (trans : string) =
    match trans.Split ' ' with
    | [|inp;_;res|] -> (parseMolecule inp |> Seq.head, parseMolecule res)
    | _ -> failwith ("could not parse transformation: "+trans)

let tupleGroupByFirst<'Key when 'Key : equality> : seq<'Key*_> -> seq<'Key*seq<_>>  = Seq.groupBy fst >> Seq.map (fun (i,li) -> (i,li |> Seq.map snd))

let transforms = input |> Seq.takeWhile (fun s -> s <> "") |> Seq.map parseTransform |> tupleGroupByFirst |> Map.ofSeq

let stringsFromOneTransform (atoms : string list) =
    let rec sFOT headAtoms tailAtoms out = 
        match tailAtoms with
        | [] -> out
        | head::tail -> 
            let withHeadTransformed =
                let (|HeadInMap|_|) map = Map.tryFind head map
                match transforms with
                | HeadInMap tList -> tList |> Seq.map (fun t -> ((headAtoms@t@tail) |> String.concat "")) |> Seq.toList
                | _ -> []
            sFOT (headAtoms@[head]) tail (withHeadTransformed@out)
    sFOT [] atoms []

start |> stringsFromOneTransform |> Seq.distinct |> Seq.length |> printfn "%A"