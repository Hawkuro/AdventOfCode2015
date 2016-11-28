let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__+ @"\D16input.txt");

let inputSue = "children: 3, cats: 7, samoyeds: 2, pomeranians: 3, akitas: 0, vizslas: 0, goldfish: 5, trees: 3, cars: 2, perfumes: 1"

let (|Integer|_|) (str: string) =
   let mutable intvalue = int 0
   if System.Int32.TryParse(str, &intvalue) then Some(intvalue)
   else None

type Sue = {children: int option; cats: int option; samoyeds: int option; pomeranians: int option; akitas: int option; vizslas: int option; goldfish: int option; trees: int option; cars: int option; perfumes: int option} with
    member this.With (str : string) input =
        match str with
        | "children" -> {this with children = Some input}
        | "cats" -> {this with cats = Some input}
        | "samoyeds" -> {this with samoyeds = Some input}
        | "pomeranians" -> {this with pomeranians = Some input}
        | "akitas" -> {this with akitas = Some input}
        | "vizslas" -> {this with vizslas = Some input}
        | "goldfish" -> {this with goldfish = Some input}
        | "trees" -> {this with trees = Some input}
        | "cars" -> {this with cars = Some input}
        | "perfumes" -> {this with perfumes = Some input}
        | _ -> failwith ("could not parse thing string: "+str)
    static member Empty = {children = None; cats = None; samoyeds = None; pomeranians = None; akitas = None; vizslas = None; goldfish = None; trees = None; cars = None; perfumes = None}

let stringToSue (str : string) =
    str.Split ',' |> Array.fold (fun (sue : Sue) (s : string) -> 
        match (s.Split([|' ';':'|],System.StringSplitOptions.RemoveEmptyEntries)) with
        | [|thing;Integer i|] -> sue.With thing i
        | _ -> failwith ("could not parse string: "+s)) Sue.Empty

let theSue = stringToSue inputSue

let parseSue str = 
    let number = 