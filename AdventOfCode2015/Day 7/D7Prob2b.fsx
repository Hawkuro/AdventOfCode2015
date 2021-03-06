﻿open System.Collections.Generic

let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__+ @"/D7input.txt");



let (|Integer|_|) (str: string) =
   let mutable intvalue = uint16 0
   if System.UInt16.TryParse(str, &intvalue) then Some(intvalue)
   else None

let readInputString (str :string) = 
    match (str.Split ' ') with
    | [|_;_;_;"->";"b"|]
    | [|_;_;"->";"b"|]
    | [|_;"->";"b"|] -> ("b",((fun x y -> x),["46065"]))
    | [|a;"AND";b;"->";outp|] -> (outp,((&&&),[a;b]))
    | [|a;"OR";b;"->";outp|] -> (outp,((|||),[a;b]))
    | [|a;"RSHIFT";b;"->";outp|] -> (outp,( (fun x y -> x >>> int32 y) ,[a;b]))
    | [|a;"LSHIFT";b;"->";outp|] -> (outp,( (fun x y -> x <<< int32 y) ,[a;b]))
    | [|"NOT";a;"->";outp|] -> (outp,((fun x y -> ~~~ x),[a]))
    | [|inp;"->";outp|] -> (outp,((fun x y -> x),[inp]))
    | _ -> failwith ("unrecognized command: "+str)


let opMap = input |> Seq.map readInputString |> Map.ofSeq
let mutable Values = new Dictionary<string,uint16>()

let rec calculate (values : IDictionary<string,uint16>) (opeMap : Map<string,((uint16 -> uint16 -> uint16)*string list)>) getValue = 
    let rec calculate' values (opeMap : Map<string,((uint16 -> uint16 -> uint16)*string list)>) getValue = 
        printfn "%s" getValue
        let (|Literal|_|) str = 
            match str with
            | Integer i -> Some i
            | s -> Some (calculate values opeMap s)
            | _ -> failwith ("Parse error: "+str)
        let command = opeMap.[getValue]
        let ans =
            match command with
            | (comm,[Literal a;Literal b]) -> comm a b
            | (comm,[Literal a]) -> comm a a
            | _ -> failwith ("Failed to execute command for "+getValue)
        values.Add (getValue,ans)
        printfn "%s" (getValue+" = "+ans.ToString())
        ans
    if values.ContainsKey getValue then values.[getValue] else calculate' values opeMap getValue

calculate (Values) opMap "a" |> printfn "%A"