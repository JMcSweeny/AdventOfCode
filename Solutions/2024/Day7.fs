namespace Soltions2024

module Day7 =
    open Common
    
    let parseLine (line: string) =
        let parts = split ": " line
        let total =  int64 parts.[0]
        let values = split " " parts.[1] |> Seq.map int64 |> Seq.toList
        total, values

    let calculateResults (value: int64) (result: int64) = [result + value; result * value]
    let calculateResultsWithConcat (value: int64) (result: int64) = [result + value; result * value; string result + string value |> int64]

    let findTotals fn (values: list<int64>) =
        let rec inner (remaining: list<int64>) (totals: list<int64>) =
            match remaining with
            | [] -> totals
            | x :: xs -> inner xs (totals |> List.collect (fn x))
        match values with
        | [] -> []
        | x :: xs -> inner xs [x]

    let hasMatch fn (total: int64, values: int64 list) =
        let totals = findTotals fn values
        Seq.exists (fun t -> t = total) totals

    [<Solution(2024, 7, 1)>]
    let part1 fileName =
        fileName
        |> readLinesAs parseLine
        |> Seq.filter (hasMatch calculateResults)
        |> Seq.sumBy fst

    [<Solution(2024, 7, 2)>]
    let part2 fileName =
        fileName
        |> readLinesAs parseLine
        |> Seq.filter (hasMatch calculateResultsWithConcat)
        |> Seq.sumBy fst


