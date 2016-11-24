// Prob 1
let input = "bgvyzdsv"

open System.Security.Cryptography
open System.Text

let md5 (data : byte array) : string =
    use md5 = MD5.Create()
    (StringBuilder(), md5.ComputeHash(data))
    ||> Array.fold (fun sb b -> sb.Append(b.ToString("x2")))
    |> string

let (|SixZeroes|NoSixZeroes|) (str : string) =
    if str.StartsWith("000000") 
    then SixZeroes 
    else NoSixZeroes

let md5Check str (i : int) = 
    match (md5 (str + i.ToString() |> System.Text.Encoding.ASCII.GetBytes)) with 
    | SixZeroes -> true
    | NoSixZeroes -> false

let nStar = 
    let rec loop x = seq { yield x; yield! loop (x + 1) }
    loop 0;;

let ans = Seq.find (md5Check input) nStar
ans |> printfn "%A"
input + ans.ToString() |> System.Text.Encoding.ASCII.GetBytes |> md5 |> printfn "%A"