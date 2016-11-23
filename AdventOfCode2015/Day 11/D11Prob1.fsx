let input = "vzbxkghb"

let getListWithout l (withOut : Set<_>) = 
    let rec getListWithout' l out = 
        match l with
        | [] -> out
        | head::tail when withOut.Contains head -> getListWithout' tail out
        | head::tail -> getListWithout' tail (head::out)
    List.rev (getListWithout' l [])

let letters = [|for c in 'a'..'z' -> c|]
let legalLetters = ['i';'o';'l'] |> Set.ofList |> getListWithout (Array.toList letters) |> List.toArray

let legalLetterMap = [for i in 1..(Array.length legalLetters) -> i-1] |> List.map (fun i -> (legalLetters.[i],i)) |> Map.ofList
let letterMap = [for i in 1..(Array.length letters) -> i-1] |> List.map (fun i -> (letters.[i],i)) |> Map.ofList

let findNextPassword (str : string) = 
    let workList = str |> Seq.toList |> List.rev

    let rec letterTrio charList =
        let isTrio a b c = letterMap.[a] = letterMap.[b]+1 && letterMap.[b] = letterMap.[c]+1 
        match charList with
        | a::b::c::tail when isTrio a b c -> true
        | a::b::c::[] -> false //Has not match above, thus is not a trio
        | a::tail -> letterTrio tail

    let (|LetterTrio|_|) charList = if letterTrio charList then Some LetterTrio else None
    
    let (|TwoDoubles|_|) charList = if (charList |> Seq.pairwise |> Seq.filter (fun (a,b)->a=b) |> Seq.distinct |> Seq.length) >= 2 then Some TwoDoubles else None

    let rec incrementPassword charList =
        match charList with
        | [] -> []
        | 'z'::tail -> 'a'::(incrementPassword tail)
        | c::tail -> (legalLetters.[legalLetterMap.[c]+1])::tail

    let rec findNextPassword' (pw : char list) =
        match pw with
        | LetterTrio & TwoDoubles -> pw
        | _ -> findNextPassword' (incrementPassword pw)

    findNextPassword' (incrementPassword workList) |> List.rev |> List.toArray |> System.String

findNextPassword input |> printfn "%A"