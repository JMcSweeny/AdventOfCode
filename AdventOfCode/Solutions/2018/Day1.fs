namespace Soltions2018

module Day1 =
    open Common

    let sumSeq seq = Seq.fold (+) 0 seq
    let runningTotal seq = Seq.scan (+) 0 seq |> Seq.skip 1

    let repeat items = seq { while true do yield! items }

    let parseInput fileName = 
        fileName
        |> readLinesAs int

    [<Solution(2018, 1, 1)>]
    let part1 fileName =
        fileName
        |> parseInput
        |> sumSeq

    [<Solution(2018, 1, 2)>]
    let part2 fileName =
        fileName
        |> parseInput
        |> repeat
        |> Seq.scan (fun (f, seen) d -> f + d, seen |> Set.add f) (0, Set.empty)
        |> Seq.find (fun (f, seen) -> seen |> Set.contains f)
        |> fst