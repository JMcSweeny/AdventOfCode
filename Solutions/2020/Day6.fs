namespace Soltions2020

module Day6 = 
    open Common
    open System

    let parseGroups = 
        split (Environment.NewLine + Environment.NewLine)
        >> Seq.map (replace Environment.NewLine " ")
        >> Seq.map (split " ")

    let findAggregate = Seq.fold (+) "" >> Seq.distinct >> Seq.length
    let findCommon = Seq.map Set.ofSeq >> Seq.reduce Set.intersect >> Set.count
        
    [<Solution(2020, 6, 1)>]
    let part1 fileName =
        fileName
        |> readText
        |> parseGroups
        |> Seq.map findAggregate
        |> Seq.sum

    [<Solution(2020, 6, 2)>]
    let part2 fileName = 
        fileName
        |> readText
        |> parseGroups
        |> Seq.map findCommon
        |> Seq.sum
