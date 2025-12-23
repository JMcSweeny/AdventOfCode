namespace Solutions2025

module Day6 =
    open Common
    open System.Text.RegularExpressions

    let parseLine (line: string) =
        Regex.Split(line, @"\s+") 
        |> Array.filter (fun s -> s <> "")

    let compute (op: string) (numbers: seq<int64>) =
        match op with
        | "*" -> numbers |> Seq.fold (*) (int64 1)
        | "+" -> numbers |> Seq.fold (+) (int64 0)
        | _ -> failwith $"Unknown operator: {op}"

    let computeProblem (problem: string[]) =
        let op = problem |> Array.last
        let numbers = problem |> Array.truncate (problem.Length - 1) |> Array.map int64
        compute op numbers

    let computeRightToLeft (numbers: List<int64>, totals: List<int64>) (col: seq<char>) =
        let digit = col |> Seq.truncate (Seq.length col - 1) |> Seq.filter (fun c -> c <> ' ') |> Seq.toArray |> System.String |> int64
        let nextNumbers = numbers @ [digit]
        let lastChar = col |> Seq.last |> string

        match lastChar with
        | " " -> nextNumbers, totals
        | _ ->  List.empty, totals @ [compute lastChar nextNumbers] 

    [<Solution(2025, 6, 1)>]
    let part1 fileName =
        fileName
        |> readLinesAs parseLine
        |> Array.transpose
        |> Array.map computeProblem
        |> Array.sum

    [<Solution(2025, 6, 2)>]
    let part2 fileName =
        fileName
        |> readLines
        |> Array.map (fun s -> s :> seq<char>)
        |> Seq.transpose
        |> Seq.rev
        |> Seq.filter (fun s -> s |> Seq.exists (fun c -> c <> ' '))
        |> Seq.fold computeRightToLeft (List.empty, List.empty)
        |> snd
        |> Seq.sum
