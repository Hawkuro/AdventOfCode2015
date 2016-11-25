let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__+ @"\D3input.txt")

let nextHouse (x,y) c = 
    match c with
    | '^' -> (x,y+1)
    | '>' -> (x+1,y)
    | 'v' -> (x,y-1)
    | '<' -> (x-1,y)
    | _ -> (x,y)
    
let visitHouses ((visited : Set<_>),location) c = 
    let nh = nextHouse location c
    (visited.Add(nh),nh)

input |> Seq.toList |> List.fold visitHouses (Set.empty.Add((0,0)),(0,0)) |> fst 
    |> Set.count
    |> printfn "%A"