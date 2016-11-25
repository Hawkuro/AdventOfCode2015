let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__+ @"\D1input.txt");

let upOrDown n c =
    match c with
    | '(' -> n+1
    | ')' -> n-1
    | _ -> n
    
let basementCheck n fp = n < 0 && fp = 0

let foundPosition n fp p = 
    match (n,fp) with
    | (a,b) when basementCheck a b -> p
    | _ -> fp

let filter (n, p, fp) c = 
    let floor = upOrDown n c
    (floor, p + 1, foundPosition floor fp p)


input |> (fun s -> [for c in s -> c]) |> List.fold filter (0,1,0) |> (fun (_,_,c)->c) |> printfn "%d"
