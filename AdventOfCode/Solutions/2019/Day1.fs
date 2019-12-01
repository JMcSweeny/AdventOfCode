namespace Soltions2019

module Day1 =
    open Common

    let calculateFuel mass = mass / 3 - 2

    let rec calculateTotalFuel mass =
        match calculateFuel mass with
            | m when m <= 0 -> 0
            | m -> m + calculateTotalFuel m

    [<Solution(2019, 1, 1)>]
    let part1 fileName = 
        fileName
        |> readLinesAs int
        |> Seq.sumBy calculateFuel

    [<Solution(2019, 1, 2)>]
    let part2 fileName =
        fileName
        |> readLinesAs int
        |> Seq.sumBy calculateTotalFuel
