let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__+ @"\D17input.txt");

let (|Integer|_|) (str: string) =
   let mutable intvalue = int 0
   if System.Int32.TryParse(str, &intvalue) then Some(intvalue)
   else None

let containers = input |> Array.map (function | Integer i -> i) |> Array.toList

let rec fitLitersCount conts amount = 
    match conts with
    | [] -> 0
    | head::tail when head > amount-> fitLitersCount tail amount
    | head::tail when head = amount -> 1 + fitLitersCount tail amount
    | head::tail when head < amount -> (fitLitersCount tail amount) + (fitLitersCount tail (amount-head))


fitLitersCount containers 150 |> printfn "%A"