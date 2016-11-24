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

let charList = [for c in input -> c]

let rec splitList (l : char list) (le : char list) (lo : char list) =
    match List.length l with
    | 0 -> (le,lo)
    | x when x % 2 = 0 -> splitList (List.tail l) (List.head l::le) lo
    | x when x % 2 = 1 -> splitList (List.tail l) le (List.head l::lo)

let splitLists = splitList charList [] []

let rec reverse li lo =
    match List.length li with
    | 0 -> lo
    | _ -> reverse (List.tail li) ((List.head li)::lo)

[reverse (fst splitLists) []; reverse (snd splitLists) []]
    |> List.map (fun aList -> aList |> List.fold visitHouses (Set.empty.Add((0,0)),(0,0)) |> fst )
    |> List.reduce Set.union
    |> Set.count
    |> printfn "%A"

