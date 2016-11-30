let input = 29000000

open System

let listOfDivisors (n:int) = 
    let sqrt = n |> (float) |> Math.Sqrt |> (int)
    [for i in 1..sqrt -> i]
    |> Seq.filter (fun i -> n % i = 0)
    |> Seq.map (fun i -> [i;n/i])
    |> Seq.collect id
    |> Seq.distinct
    |> Seq.filter (fun i -> n/i <= 50)

Seq.initInfinite (fun i -> listOfDivisors i |> Seq.sum |> (*) 11) |> Seq.findIndex (fun i -> i >= input) |> printfn "%A"