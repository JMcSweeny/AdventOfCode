namespace Soltions2019

module Day1 =
    open Common

    let calculateFuel mass = mass / 3 - 2

    let calculateTotalFuel mass =
        let rec calc mass = 
            match calculateFuel mass with
                | m when m <= 0 -> mass
                | m -> mass + calc m
        calc (calculateFuel mass)

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
