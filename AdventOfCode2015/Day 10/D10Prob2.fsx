let input = "3113322113" // = Bi
let atoms = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__+ @"\atoms.txt");
let iterations = 50

type atomInfo = {content : string; evolution : string list}

let parseAtom (str : string) = 
    match List.ofArray (str.Split ' ') with
    | name::content::evolution -> (name,{content = content; evolution = evolution})
    | _ -> failwith ("could not parse "+str)
     
let atomMap = atoms |> Seq.map parseAtom |> Map.ofSeq

let (|Found|_|) key map =
  map
  |> Map.tryFind key
  |> Option.map (fun x -> x, Map.remove key map)

let lookAndSay (multiSet : Map<string,int>) = 
    let updateKey number (outputSet : Map<string,int>) key = 
        match outputSet with
        | Found key (value, rest) -> rest.Add(key,value+number)
        | _ -> outputSet.Add(key,number)
    let rec lookAndSay' (multiSet : (string*int) list) (outputSet : Map<string,int>) =
        match multiSet with
        | [] -> outputSet
        | (name,number)::tail -> lookAndSay' tail (atomMap.[name].evolution |> List.fold (updateKey number) outputSet)
    lookAndSay' (Map.toList multiSet) (Map.ofSeq [])
            

let startingAtom str = atomMap |> Seq.find (fun a -> a.Value.content = str) |> (fun a -> a.Key)

[for i in 1..iterations -> 0 ]
|> List.fold (fun x _ -> lookAndSay x) (Map.ofList [(startingAtom input,1)])
|> Map.toList
|> List.map (fun (name,number) -> (Seq.length atomMap.[name].content)*number)
|> Seq.sum
|> printfn "%A"