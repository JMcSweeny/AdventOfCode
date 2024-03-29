﻿namespace Soltions2022

module Day2 =
    open Common

    let calculateScore line =
        match line with
        | "A X" -> 4
        | "A Y" -> 8
        | "A Z" -> 3
        | "B X" -> 1
        | "B Y" -> 5
        | "B Z" -> 9
        | "C X" -> 7
        | "C Y" -> 2
        | "C Z" -> 6
        | _ -> 0

    let calculateScoreTwo line =
        match line with
        | "A X" -> 3
        | "A Y" -> 4
        | "A Z" -> 8
        | "B X" -> 1
        | "B Y" -> 5
        | "B Z" -> 9
        | "C X" -> 2
        | "C Y" -> 6
        | "C Z" -> 7
        | _ -> 0

    [<Solution(2022, 2, 1)>]
    let part1 fileName =
        fileName
        |> readLinesAs calculateScore
        |> Seq.sum

    [<Solution(2022, 2, 2)>]
    let part2 fileName =
        fileName
        |> readLinesAs calculateScoreTwo
        |> Seq.sum
