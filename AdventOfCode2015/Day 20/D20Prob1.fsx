let input = 29000000

open System

let listOfDivisors (n:int) = 
    let sqrt = n |> (float) |> Math.Sqrt |> Math.Floor |> (int)
    [for i in 1..sqrt -> i]
    |> Seq.filter (fun i -> n % i = 0)
    |> Seq.map (fun i -> [i;n/i])
    |> Seq.collect id
    |> Seq.distinct

Seq.initInfinite (fun i -> listOfDivisors i |> Seq.sum |> (*) 10) |> Seq.findIndex (fun i -> i >= input) |> printfn "%A"