namespace Soltions2022

module Day6 =
    open Common

    let hasDuplicates (window: int) (chars: seq<char>) =
        chars |> Seq.distinct |> Seq.length < window

    let getMarker (window: int) (buffer: string) =
        buffer
        |> Seq.windowed window
        |> Seq.takeWhile (hasDuplicates window)
        |> Seq.length
        |> (fun l -> l + window)


    [<Solution(2022, 6, 1)>]
    let part1 fileName =
        fileName
        |> readText
        |> getMarker 4

    [<Solution(2022, 6, 2)>]
    let part2 fileName =
        fileName
        |> readText
        |> getMarker 14