let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__+ @"\D18input.txt");

let (|On|Off|) c =
    match c with
    | '.' -> Off
    | '#' -> On


let parseLines (lines : seq<string>) =
    let parseLine lineNr (line: string) = 
        let rec parseLine' charNr (charList : char list) out =
            match charList with
            | [] -> out
            | On::tail -> parseLine' (charNr+1) tail (((lineNr,charNr),true)::out)
            | Off::tail -> parseLine' (charNr+1) tail (((lineNr,charNr),false)::out)
        parseLine' 0 (Seq.toList line) []
    let rec parseLines' (lines:string list) lineNr out =
        match lines with
        | [] -> out
        | head::tail -> parseLines' tail (lineNr+1) ((parseLine lineNr head)@out)
    parseLines' (Seq.toList lines) 0 []

let step0List = parseLines input

let nextStep (pixelList : ((int*int)*bool) list) =
    let pixelMap = Map.ofList pixelList
    let maxX = pixelList |> Seq.map (fst >> fst) |> Seq.max
    let maxY = pixelList |> Seq.map (fst >> snd) |> Seq.max
    let calculatePixel (x,y) =
        let pixelValue p =
            match p with
            | (a,b) when (a = 0 || a = maxX) && (b = 0 || b = maxY) -> true
            | _ -> 
                let (|PInMap|_|) map = Map.tryFind p map
                match pixelMap with
                | PInMap s -> s
                | _ -> false
        let surroundingOn =
            [for i in -1..1 -> [for j in -1..1 -> (x+i,y+j)]] 
            |> List.collect id |> Seq.except [(x,y)] |> Seq.map pixelValue
            |> Seq.filter id |> Seq.length
        if pixelMap.[(x,y)] 
            then surroundingOn = 2 || surroundingOn = 3 || ((x = 0 || x = maxX) && (y = 0 || y = maxY))
            else surroundingOn = 3 || ((x = 0 || x = maxX) && (y = 0 || y = maxY))
    pixelList |> List.map (fun (ind,state) -> (ind,calculatePixel ind))

let onItselfUnfolder func item = 
    let ans = func item
    Some(item,ans)

Seq.unfold (onItselfUnfolder nextStep) step0List |> Seq.item 100 |> Seq.map snd |> Seq.filter id |> Seq.length |> printfn "%A"
