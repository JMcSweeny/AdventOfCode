namespace Solutions2022

module Day1 =
    open Common

    let calculateCalorie (curr, totals) calorie =
        match calorie with
            | "" -> (0, curr :: totals)
            | _ -> (curr + (int calorie), totals)

    let topThreeTotal = Seq.sortDescending >> Seq.take 3 >> Seq.sum

    let parseInput fileName = 
        fileName
        |> readLines

    [<Solution(2022, 1, 1)>]
    let part1 fileName =
        fileName
        |> parseInput
        |> Seq.fold calculateCalorie (0, List.Empty)
        |> snd
        |> Seq.max

    [<Solution(2022, 1, 2)>]
    let part2 fileName =
        fileName
        |> parseInput
        |> Seq.fold calculateCalorie (0, List.Empty)
        |> snd
        |> topThreeTotal
