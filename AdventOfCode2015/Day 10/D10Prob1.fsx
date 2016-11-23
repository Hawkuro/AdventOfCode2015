let input = "3113322113"
let iterations = 40

let lookAndSay str = 
    let rec lookAndSay' strTail out (currentChar : char) currentCount =
        match strTail with
        | [] -> out + (currentCount.ToString()) + new string [|currentChar|]
        | head::tail when head = currentChar -> lookAndSay' tail out currentChar (currentCount+1)
        | head::tail -> lookAndSay' tail (out + (currentCount.ToString()) + new string [|currentChar|]) head 1
    match (Seq.toList str) with
    | head::tail -> lookAndSay' tail "" head 1
    | _ -> failwith "empty string"

[for i in 1..iterations -> input ]
|> List.fold (fun x _ -> lookAndSay x) input
|> Seq.length
|> printfn "%A"