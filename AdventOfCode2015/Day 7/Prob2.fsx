open System.Collections.Generic

let input = System.IO.File.ReadAllLines(@"C:\Users\hawku\Documents\Visual Studio 2015\Projects\AdventOfCode2015\AdventOfCode2015\Day 7\input.txt");



let (|Integer|_|) (str: string) =
   let mutable intvalue = uint16 0
   if System.UInt16.TryParse(str, &intvalue) then Some(intvalue)
   else None

let readInputString (str :string) = 
    match (str.Split ' ') with
    | [|_;_;_;"->";"b"|]
    | [|_;_;"->";"b"|]
    | [|_;"->";"b"|] -> ("b",((fun x y -> x),["a"]))
    | [|a;"AND";b;"->";outp|] -> (outp,((&&&),[a;b]))
    | [|a;"OR";b;"->";outp|] -> (outp,((|||),[a;b]))
    | [|a;"RSHIFT";b;"->";outp|] -> (outp,( (fun x y -> x >>> int32 y) ,[a;b]))
    | [|a;"LSHIFT";b;"->";outp|] -> (outp,( (fun x y -> x <<< int32 y) ,[a;b]))
    | [|"NOT";a;"->";outp|] -> (outp,((fun x y -> ~~~ x),[a]))
    | [|inp;"->";outp|] -> (outp,((fun x y -> x),[inp]))
    | _ -> failwith ("unrecognized command: "+str)


let opMap = input |> Seq.map readInputString |> Map.ofSeq

let rec calculate (values : IDictionary<string,uint16>) (opeMap : Map<string,((uint16 -> uint16 -> uint16)*string list)>) getValue actuallyCalc = 
    let rec calculate' values (opeMap : Map<string,((uint16 -> uint16 -> uint16)*string list)>) getValue = 
//        printfn "%s" getValue
        let (|Literal|_|) str = 
            match str with
            | Integer i -> Some i
            | s -> Some (calculate values opeMap s false)
            | _ -> failwith ("Parse error: "+str)
        let command = opeMap.[getValue]
        let ans =
            match command with
            | (comm,[Literal a;Literal b]) -> comm a b
            | (comm,[Literal a]) -> comm a a
            | _ -> failwith ("Failed to execute command for "+getValue)
        if not (values.ContainsKey getValue)
        then values.Add (getValue,ans)
        else printfn "%s" (getValue+" = "+ans.ToString()+" or "+values.[getValue].ToString())
        ans
    if not actuallyCalc && values.ContainsKey getValue then values.[getValue] else calculate' values opeMap getValue


[for i in 0..65535 -> (uint16 i,calculate (System.Linq.Enumerable.ToDictionary([("a",uint16 i)], fst, snd)) opMap "a" true)] |> Seq.find (fun a -> (fst a) = (snd a)) |> printfn "%A"