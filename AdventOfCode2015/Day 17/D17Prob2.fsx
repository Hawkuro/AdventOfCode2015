let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__+ @"\D17input.txt");

let (|Integer|_|) (str: string) =
   let mutable intvalue = int 0
   if System.Int32.TryParse(str, &intvalue) then Some(intvalue)
   else None

let containers = input |> Array.map (function | Integer i -> i) |> Array.toList

let fitLitersCount conts amount = 
    let rec fitLitersCount' conts amount contNr =
        match conts with
        | [] -> []
        | head::tail when head > amount-> fitLitersCount' tail amount contNr
        | head::tail when head = amount -> (contNr+1)::(fitLitersCount' tail amount contNr)
        | head::tail when head < amount -> (fitLitersCount' tail amount contNr)@(fitLitersCount' tail (amount-head) (contNr+1))
    fitLitersCount' conts amount 0

let contNumbers = fitLitersCount containers 150 
let minContCount = List.min contNumbers
contNumbers |> List.filter ((=) minContCount) |> List.length |> printfn "%A"