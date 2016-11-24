let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__+ @"\D5input.txt") |> Seq.toList;

let vowels = "aeiou" |> Seq.toList

let (|Vowel|Consonant|) (c : char) = if Seq.exists ((=) c) vowels then Vowel else Consonant

let countVowels str = 
    let isVowel c =
        match c with
        | Vowel -> true
        | Consonant -> false
    str |> Seq.filter isVowel |> Seq.length
//    let rec countVowels' n str =
//        match Seq.length str with
//        | 0 -> n
//        | _ -> 
//            match List.head str with
//            | Vowel -> countVowels' (n+1) (List.tail str)
//            | Consonant -> countVowels' n (List.tail str)
//    countVowels' 0 str

let (|ThreeVowels|_|) str = if countVowels str >= 3 then Some ThreeVowels else None


let hasDoubleLetter str = Seq.pairwise str |> Seq.map (fun (a,b) -> a=b) |> Seq.reduce (||) 

let (|DoubleLetter|_|) str = if hasDoubleLetter str then Some DoubleLetter else None


let badPairs = ["ab";"cd";"pq";"xy"]

let (|BadPair|_|) str = if Seq.exists ((=) str) badPairs then Some BadPair else None

let isBadPair str = 
    match str with
    | BadPair -> true
    | _ -> false

let toString : char seq -> string = Seq.map string >> String.concat ""

let hasBadPair (str : char list) = Seq.pairwise str |> Seq.map (fun (a,b) -> [a;b] |> toString) |> Seq.exists isBadPair

let (|HasBadPair|HasNoBadPairs|) str = if hasBadPair str then HasBadPair else HasNoBadPairs


let isNice (str : string) = 
    match Seq.toList str with
    | ThreeVowels & DoubleLetter & HasNoBadPairs -> true
    | _ -> false

input |> Seq.filter isNice |> Seq.length |> printfn "%A"