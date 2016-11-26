let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__+ @"\D15input.txt");
let inputNumberOfSpoons = 100

let (|Integer|_|) (str: string) =
   let mutable intvalue = int 0
   if System.Int32.TryParse(str, &intvalue) then Some(intvalue)
   else None

type IngredientData = {capacity : int; durability : int; flavor : int; texture : int; calories : int}

let (|IngData|_|) (str : string) =
    match ((str.Substring 1).Replace(",","").Split ' ') with
    | [|"capacity"; Integer cap; "durability"; Integer dur; "flavor"; Integer flav; "texture";Integer text; "calories"; Integer cal|] -> Some({capacity = cap; durability = dur; flavor = flav; texture = text; calories = cal})
    | _ -> None

let parseIngredientString (str : string) = 
    match (str.Split ':') with
    | [|name;IngData data|] -> (name,data)
    | _ -> failwith ("Could not parse string: "+str)

let IngList = input |> Seq.map parseIngredientString
let IngMap = IngList |> Map.ofSeq

let calculateScore (ingProports : seq<string*int>) = 
    let getScore (ingredientSelector : IngredientData -> int) =
        ingProports
        |> Seq.map (fun (name,proport) -> (ingredientSelector IngMap.[name])*proport)
        |> Seq.sum
        |> max 0
    [(fun id -> id.calories); (fun id -> id.capacity); (fun id -> id.durability); (fun id -> id.flavor); (fun id -> id.texture)]
    |> List.map getScore
    |> function
        | 500::tail -> tail |> List.reduce (*)
        | _ -> 0

let getAllProportions total typeList =
    let rec getAllProportions' total typeList prop =
        match typeList with
        | head::[] -> [(head,total)::prop]
        | head::tail -> [for i in 0..total -> (getAllProportions' (total-i) tail ((head,i)::prop))] |> List.collect id
    getAllProportions' total (Seq.toList typeList) []

IngList |> Seq.map fst |> getAllProportions inputNumberOfSpoons
|> Seq.map calculateScore |> Seq.max
|> printfn "%A"
