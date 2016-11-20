let input = System.IO.File.ReadAllLines(@"C:\Users\hawku\Documents\Visual Studio 2015\Projects\AdventOfCode2015\AdventOfCode2015\Day 7\input.txt");


let calculate (opeMap : Map<string,_>) getValue = opeMap.[getValue](opeMap) 

let (|Integer|_|) (str: string) =
   let mutable intvalue = uint16 0
   if System.UInt16.TryParse(str, &intvalue) then Some(intvalue)
   else None

let (|Not|_|) (str: string) =
    match (str.Split ' ') with
    | [|"NOT";Integer inp;"->";outp|] -> Some(outp,(fun opeMap -> ~~~ inp))
    | [|"NOT";inp;"->";outp|] -> Some(outp,(fun opeMap -> ~~~ (calculate opeMap inp)))
    | _ -> None

let (|Or|_|) (str: string) =
    match (str.Split ' ') with
    | [|Integer a;"OR";Integer b;"->";outp|] -> Some(outp,(fun opeMap -> a ||| b))
    | [|a;"OR";Integer b;"->";outp|] -> Some(outp,(fun opeMap -> (calculate opeMap a) ||| b))
    | [|Integer a;"OR";b;"->";outp|] -> Some(outp,(fun opeMap -> a ||| (calculate opeMap b)))
    | [|a;"OR";b;"->";outp|] -> Some(outp,(fun opeMap -> (calculate opeMap a) ||| (calculate opeMap b)))
    | _ -> None

let (|And|_|) (str: string) =
    match (str.Split ' ') with
    | [|Integer a;"AND";Integer b;"->";outp|] -> Some(outp,(fun opeMap -> a &&& b))
    | [|a;"AND";Integer b;"->";outp|] -> Some(outp,(fun opeMap -> (calculate opeMap a) &&& b))
    | [|Integer a;"AND";b;"->";outp|] -> Some(outp,(fun opeMap -> a &&& (calculate opeMap b)))
    | [|a;"AND";b;"->";outp|] -> Some(outp,(fun opeMap -> (calculate opeMap a) &&& (calculate opeMap b)))
    | _ -> None

let (|RShift|_|) (str: string) =
    match (str.Split ' ') with
    | [|Integer a;"RSHIFT";Integer b;"->";outp|] -> Some(outp,(fun opeMap -> a >>> int32 b))
    | [|a;"RSHIFT";Integer b;"->";outp|] -> Some(outp,(fun opeMap -> (calculate opeMap a) >>> int32 b))
    | [|Integer a;"RSHIFT";b;"->";outp|] -> Some(outp,(fun opeMap -> a >>> int32 (calculate opeMap b)))
    | [|a;"RSHIFT";b;"->";outp|] -> Some(outp,(fun opeMap -> (calculate opeMap a) >>> int32 (calculate opeMap b)))
    | _ -> None

let (|LShift|_|) (str: string) =
    match (str.Split ' ') with
    | [|Integer a;"LSHIFT";Integer b;"->";outp|] -> Some(outp,(fun opeMap -> a <<< int32 b))
    | [|a;"LSHIFT";Integer b;"->";outp|] -> Some(outp,(fun opeMap -> (calculate opeMap a) <<< int32 b))
    | [|Integer a;"LSHIFT";b;"->";outp|] -> Some(outp,(fun opeMap -> a <<< int32 (calculate opeMap b)))
    | [|a;"LSHIFT";b;"->";outp|] -> Some(outp,(fun opeMap -> (calculate opeMap a) <<< int32 (calculate opeMap b)))
    | _ -> None

let (|Input|_|) (str: string) =
    match (str.Split ' ') with
    | [|Integer i;"->";outp|] -> Some(outp,(fun opeMap -> i))
    | [|inp;"->";outp|] -> Some(outp,(fun opeMap -> calculate opeMap inp))
    | _ -> None

let readInputString str = 
    match str with
    |Not op|Or op|And op|RShift op|LShift op|Input op -> op
    | _ -> failwith ("unrecognized command: "+str)

let opMap = input |> Seq.map readInputString |> Map.ofSeq

