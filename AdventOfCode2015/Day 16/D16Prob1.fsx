let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__+ @"\D16input.txt");

let inputSue = "children: 3, cats: 7, samoyeds: 2, pomeranians: 3, akitas: 0, vizslas: 0, goldfish: 5, trees: 3, cars: 2, perfumes: 1"

let (|Integer|_|) (str: string) =
   let mutable intvalue = int 0
   if System.Int32.TryParse(str, &intvalue) then Some(intvalue)
   else None

let parseSue (str : string) = 
    let parseThing (s : string) =
        match s.Split([|':';' '|],System.StringSplitOptions.RemoveEmptyEntries) with
        | [|thing;Integer number|] -> (thing,number)
        | _ -> failwith ("couldn't parse string: "+str)
    str.Split ',' |> Array.map parseThing

let theSue = parseSue inputSue |> Map.ofArray

let readSue (sueString : string) =
    let [|firstBit;theRest|] = sueString.Split([|':'|],2)
    match (firstBit.Split ' ') with
    | [|"Sue";Integer index|] -> (index, parseSue theRest)
    | _ -> failwith ("couldn't parse strings: "+firstBit+" | "+theRest)

let isTheSue (sue : (string*int)[]) =
    sue |> Array.map (fun (thing,number) -> theSue.[thing] = number) |> Seq.reduce (&&)

input |> Seq.map readSue |> Seq.map (fun (index,data) -> (index,isTheSue data)) |> Seq.find snd |> fst |> printfn "%A"