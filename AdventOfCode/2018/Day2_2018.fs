module Day2_2018

open Common

let chooseTwosAndThrees (_, count) =
    match count with 
        | 2 -> Some 2
        | 3 -> Some 3
        | _ -> None

let collectTwosAndThrees = Seq.countBy id >> Seq.choose chooseTwosAndThrees >> Seq.distinct

let replaceAtIndex (s: string) (n: int) = String.mapi (fun i c -> if i = n then '*' else c) s

[<Solution(2018, 2, 1)>]
let part1 fileName = 
    readLines fileName
    |> Seq.collect collectTwosAndThrees
    |> Seq.countBy id
    |> Seq.fold (fun product (_, count) -> count * product) 1

[<Solution(2018, 2, 2)>]
let part2 fileName = 
    readLines fileName
    |> Seq.collect (fun str -> Seq.init (String.length str) (replaceAtIndex str))
    |> Seq.groupBy id
    |> Seq.pick (fun (k, v) -> if Seq.length v = 2 then Some(k) else None)
    |> String.filter (fun c -> c <> '*')
    
    