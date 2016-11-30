let inputHP = 100
let inputBossHP = 104
let inputBossDamage = 8
let inputBossArmor = 1

type Item = {cost:int;damage:int;armor:int} with
    static member fromTuple (a,b,c) = {cost=a;damage=b;armor=c;}

let weapons = [
    Item.fromTuple (8, 4, 0);
    Item.fromTuple (10, 5, 0);
    Item.fromTuple (25, 6, 0);
    Item.fromTuple (40, 7, 0);
    Item.fromTuple (74, 8, 0)
]

let armors = [
    Item.fromTuple (13, 0, 1); 
    Item.fromTuple (31, 0, 2) 
    Item.fromTuple (53, 0, 3); 
    Item.fromTuple (75, 0, 4); 
    Item.fromTuple (102, 0, 5);
]

let rings = [
    Item.fromTuple (25, 1, 0); 
    Item.fromTuple (50, 2, 0); 
    Item.fromTuple (100, 3, 0); 
    Item.fromTuple (20, 0, 1); 
    Item.fromTuple (40, 0, 2); 
    Item.fromTuple (80, 0, 3);
]

let playerWins ((bossHP,bossDamage,bossArmor) as boss) ((playerHP,playerDamage,playerArmor) as player) =
    let hitDmgToBoss = max 1 (playerDamage-bossArmor)
    let hitDmgToPlayer = max 1 (bossDamage-playerArmor)
    //If there is a remainder between the damage dealt and the HP,
    // an extra turn is required to push the character over the edge
    let playerExtraTurn = if bossHP % hitDmgToBoss > 0 then 1 else 0
    let bossExtraTurn = if playerHP % hitDmgToPlayer > 0 then 1 else 0
    let turnsToKillBoss = playerExtraTurn + (bossHP / hitDmgToBoss)
    let turnsToKillPlayer = bossExtraTurn + (playerHP / hitDmgToPlayer)
    turnsToKillPlayer >= turnsToKillBoss


let getPairNoDubs aList = [for i in aList -> [for j in aList -> [i;j]]] |> List.collect id |> List.except [for i in aList -> [i;i]]

let (equipmentPerms : Item list list) =
    let weaponSets = weapons |> List.map (fun w -> [w])
    let armorSets = []::(armors |> List.map (fun a -> [a]))
    let ringSets = []::(rings |> List.map (fun r -> [r]))@(getPairNoDubs rings)
    [for wS in weaponSets -> [for aS in armorSets -> [for rS in ringSets -> wS@aS@rS]]] |> List.collect (List.collect id)

let winsWithSet ((bossHP,bossDamage,bossArmor) as boss) playerHP (set:Item list) =
    let damage = set |> List.map (fun i -> i.damage) |> List.sum
    let armor = set |> List.map (fun i -> i.armor) |> List.sum
    playerWins boss (playerHP,damage,armor)

equipmentPerms |> List.filter ((winsWithSet (inputBossHP,inputBossDamage,inputBossArmor) inputHP ) >> not)
|> List.map (List.map (fun i -> i.cost) >> List.sum) |> List.max |> printfn "%A"