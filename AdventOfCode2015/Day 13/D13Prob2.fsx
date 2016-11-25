let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__+ @"\D13input.txt");

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
    match (str.Split [|' ';'.'|]) with 
    | [|a;"would";"gain";Integer pts;_;_;_;_;_;_;b;_|] -> (Set.ofList [a;b], pts)
    | [|a;"would";"lose";Integer pts;_;_;_;_;_;_;b;_|] -> (Set.ofList [a;b], -pts)
    | _ -> failwith ("Couldn't parse string "+str)

let happinesses = input |> Seq.map parseInputString

let (|Found|_|) key map =
  map
  |> Map.tryFind key
  |> Option.map (fun x -> x, Map.remove key map)

let sumMap inputSeq =
    let rec sumMap' inputList map =
        match inputList with
        | [] -> map
        | head::tail ->
            match map with
            | Found (fst head) (value, restOfMap) -> sumMap' tail (restOfMap.Add(fst head,value+snd head))
            | _ -> sumMap' tail (map.Add head)
    sumMap' (Seq.toList inputSeq) Map.empty

let people = "me"::(happinesses |> Seq.map fst |> Seq.map Set.toSeq |> Seq.collect id |> Seq.distinct |> Seq.toList) |> List.toSeq

let happinessDiffMap = [for a in Seq.tail people -> (Set.ofList ["me";a],0)]|> Seq.fold (fun (map: Map<Set<string>,int>) item -> map.Add item) (sumMap happinesses)

let calculateTotalHappiness (dists : Map<Set<string>,int>) (perm : string seq) = 
    ((Seq.last perm,Seq.head perm)::(Seq.pairwise perm |> Seq.toList))
    |> Seq.map (fun pair -> pair |> (fun (x,y) -> [x;y]) |> Set.ofList)
    |> Seq.map (fun s -> dists.[s]) |> Seq.sum
    

people |> getPermutations |> Seq.map (calculateTotalHappiness happinessDiffMap) |> Seq.max |> printfn "%A"
