let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__+ @"/D5input.txt") |> Seq.toList;
let toString : char seq -> string = Seq.map string >> String.concat ""


let checkRemovedPairs (pred : char seq -> char*char -> bool) (str : char seq) = 
    let rec checkRemovedPairs' (head : char seq) (pair : char*char) (tail:char seq) (result : bool list) =
        match (Seq.toList head,pair,Seq.toList tail) with
        | (_,_,[]) -> (pred head pair)::result
        | (h,(f,s),th::t) -> checkRemovedPairs' (h @ [f]) (s,th) t ((pred (h@'-'::th::t) pair)::result)
    match Seq.toList str with
    | a::b::t -> checkRemovedPairs' [] (a,b) t []
    | _ -> []

let containsPair str pair =
    Seq.pairwise str |> Seq.exists ((=) pair)

let (|HasDoublePair|_|) str = if checkRemovedPairs containsPair str |> Seq.reduce (||) then Some HasDoublePair else None

 
let tripletwise (l :  seq<char>) = 
    let rec tripletwise' (l : char list) o = 
        match l with 
        | a::b::c::tail -> tripletwise' (b::c::tail) ((toString [a;b;c])::o)
        | _ -> o
    tripletwise' (Seq.toList l) []

let isNiceTriplet (trip : string) = 
    match Seq.toList trip with
    | [a;_;b] when a=b -> true
    | _ -> false

let (|HasNiceTriplet|_|) (str: char list) =
    if (str |> tripletwise |> Seq.exists isNiceTriplet)
    then Some HasNiceTriplet
    else None


let isNice (str : string) = 
    match Seq.toList str with
    | HasNiceTriplet & HasDoublePair -> true
    | _ -> false

input |> Seq.filter isNice |> Seq.length |> printfn "%A"