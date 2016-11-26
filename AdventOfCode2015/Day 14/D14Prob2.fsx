let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__+ @"\D14input.txt");
let inputTime = 2503

type Reindeer = {name : string; speed: int; endurance: int; restTime: int }

let (|Integer|_|) (str: string) =
   let mutable intvalue = int 0
   if System.Int32.TryParse(str, &intvalue) then Some(intvalue)
   else None

let parseReindeerString (str: string) = 
    match (str.Split [|' ';'.'|]) with
    | [|name;"can";"fly";Integer speed;"km/s";"for";Integer endurance;_;_;_;_;_;_;Integer restTime;_;_|] ->
        {name = name;speed = speed;endurance = endurance;restTime=restTime}
    | _ -> failwith ("Could not parse string: "+str)

let calculateReindeerDistance time (reindeer:Reindeer) =
    let calculateRemainderDistance remainder =
        reindeer.speed*(min reindeer.endurance remainder)
    let spurtDist = reindeer.speed*reindeer.endurance
    let spurtLength = reindeer.endurance + reindeer.restTime
    spurtDist*(time / spurtLength) + calculateRemainderDistance (time % spurtLength)

let reindeers = input |> Seq.map parseReindeerString

let (|Secondis|_|) a (b,c) = if a = c then Some (b,c) else None

seq {for i in 1..inputTime -> seq {for r in reindeers -> (r.name, calculateReindeerDistance i r)}}
|> Seq.map (fun reindeerLocations -> reindeerLocations |> Seq.maxBy snd |> (fun max -> Seq.filter (fun rl -> (snd rl) = (snd max)) reindeerLocations))
|> Seq.collect id |> Seq.countBy fst |> Seq.map snd |> Seq.max |> printfn "%A"