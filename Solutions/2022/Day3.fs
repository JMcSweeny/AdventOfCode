namespace Solutions2022

module Day3 =
    open Common

    let priorityValues = ['a'..'z'] @ ['A'..'Z']

    let toPriority item =
        Seq.findIndex ((=) item) priorityValues + 1

    [<Solution(2022, 3, 1)>]
    let part1 fileName =
        fileName
        |> readLinesAs (Seq.splitInto 2)
        |> Seq.sumBy (Seq.map Set >> Set.intersectMany >> Seq.sumBy toPriority)

    [<Solution(2022, 3, 2)>]
    let part2 fileName =
        fileName
        |> readLines
        |> Seq.chunkBySize 3
        |> Seq.collect (Seq.map Set >> Set.intersectMany)
        |> Seq.sumBy toPriority
