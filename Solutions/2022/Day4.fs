﻿namespace Soltions20122

module Day4 =
    open Common

    let toRange assignment = [(Seq.min assignment)..(Seq.max assignment)]

    let parseAssignment = split "-" >> Seq.map int >> toRange >> Set.ofSeq

    let parseAssignments = split "," >> Seq.map parseAssignment

    let fullyContains assignments = 
        let overlap = Set.intersectMany assignments

        match Seq.tryFind ((=) overlap) assignments with
        | None -> false
        | _ -> true


    [<Solution(2022, 4, 1)>]
    let part1 fileName =
        fileName
        |> readLinesAs parseAssignments
        |> Seq.filter fullyContains
        |> Seq.length

    [<Solution(2022, 4, 2)>]
    let part2 fileName =
        fileName
        |> readLinesAs parseAssignments
        |> Seq.filter (Set.intersectMany >> (fun s -> not (s |> Set.isEmpty)))
        |> Seq.length
