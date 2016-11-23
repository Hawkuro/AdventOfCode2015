let input = System.IO.File.ReadAllLines(@"C:\Users\hawku\Documents\Visual Studio 2015\Projects\AdventOfCode2015\AdventOfCode2015\Day 6\D6input.txt");

let mutable baseGrid = List.toArray [for i in 0..999 -> List.toArray [for i in 0..999 -> ref false]]

let (|Integer|_|) (str: string) =
   let mutable intvalue = 0
   if System.Int32.TryParse(str, &intvalue) then Some(intvalue)
   else None

let stringToTuple (str : string) = 
    match (str.Split ',') with
    | [|Integer x;Integer y|] -> (x,y)
    | _ -> (-1,-1)

let turnOn (x1,y1) (x2,y2) = [for i in x1..x2->i] |> Seq.collect (fun i -> [for j in y1..y2 -> (i,j)]) |> Seq.iter (fun (i,j) -> baseGrid.[i].[j] := true)

let turnOff (x1,y1) (x2,y2) = [for i in x1..x2->i] |> Seq.collect (fun i -> [for j in y1..y2 -> (i,j)]) |> Seq.iter (fun (i,j) -> baseGrid.[i].[j] := false)

let toggle (x1,y1) (x2,y2) = [for i in x1..x2->i] |> Seq.collect (fun i -> [for j in y1..y2 -> (i,j)]) |> Seq.iter (fun (i,j) -> baseGrid.[i].[j] := not !baseGrid.[i].[j])

let chooseCommand (str : string) =
    match (str.Split ' ') with
    | [|"turn";"on";coords1;"through";coords2|] -> turnOn (stringToTuple coords1) (stringToTuple coords2)
    | [|"turn";"off";coords1;"through";coords2|] -> turnOff (stringToTuple coords1) (stringToTuple coords2)
    | [|"toggle";coords1;"through";coords2|] -> toggle (stringToTuple coords1) (stringToTuple coords2)

input |> Seq.iter chooseCommand
baseGrid |> Seq.map (Seq.map (fun a-> !a)) |> Seq.collect (fun a-> a) |> Seq.filter ((=) true) |> Seq.length |> printfn "%A"