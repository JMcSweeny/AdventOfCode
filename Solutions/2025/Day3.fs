namespace Solutions2025

module Day3 =
    open Common

    let highestJoltage (batteryLength: int) (bank: list<char>) =
        let rec findHighestJoltage (bank: list<char>) (joltages: list<char>) = 
            match batteryLength - joltages.Length with
            | 0 -> joltages |> List.toArray |> System.String |> int64
            | remainingJoltages ->
                let maxSearchIndex = bank.Length - remainingJoltages
                let splitIndex, nextJoltage =
                     bank 
                    |> List.take (maxSearchIndex + 1)
                    |> List.indexed
                    |> List.maxBy snd
                let nextBank = bank |> List.skip (splitIndex + 1)
                findHighestJoltage nextBank (joltages @ [nextJoltage])
        findHighestJoltage bank List.empty

    let highestJoltageWithTwoBatteries = highestJoltage 2
    let highestJoltageWithTwelveBatteries = highestJoltage 12

    [<Solution(2025, 3, 1)>]
    let part1 fileName =
        fileName
        |> readLinesAs Seq.toList
        |> Seq.map highestJoltageWithTwoBatteries
        |> Seq.sum

    [<Solution(2025, 3, 2)>]
    let part2 fileName =
        fileName
        |> readLinesAs Seq.toList
        |> Seq.map highestJoltageWithTwelveBatteries
        |> Seq.sum