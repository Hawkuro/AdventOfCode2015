let input = System.IO.File.ReadAllLines(@"C:\Users\hawku\Documents\Visual Studio 2015\Projects\AdventOfCode2015\AdventOfCode2015\Day 9\D8input.txt");

let (|EmptySet|_|) set = if Set.isEmpty set then Some EmptySet else None

let getPermutations l = 
    let rec getPermutations' (l : Set<_>) o = 
        match l with
        | EmptySet -> [o]
        | _ -> [for e in l -> getPermutations' (l.Remove e) (e::o) ] |> Seq.collect id |> List.ofSeq
    getPermutations' (Set.ofSeq l) []


let (|Integer|_|) (str: string) =
   let mutable intvalue = int 0
   if System.Int32.TryParse(str, &intvalue) then Some(intvalue)
   else None

let parseInputString (str : string) =
    match (str.Split ' ') with 
    | [|a;"to";b;"=";Integer dist|] -> (Set.ofList [a;b], dist)
    | _ -> failwith ("Couldn't parse string "+str)

let distances = input |> Seq.map parseInputString
let distanceMap = Map.ofSeq distances

let locations = distances |> Seq.map fst |> Seq.map Set.toSeq |> Seq.collect id |> Seq.distinct

let calculateTotalDistance (dists : Map<Set<string>,int>) (perm : string seq) = 
    Seq.pairwise perm
    |> Seq.map (fun pair -> pair |> (fun (x,y) -> [x;y]) |> Set.ofList)
    |> Seq.map (fun s -> dists.[s]) |> Seq.sum
    

locations |> getPermutations |> Seq.map (calculateTotalDistance distanceMap) |> Seq.max |> printfn "%A"