namespace Soltions$year$

module Day$day$ =
    open Common

    [<Solution($year$, $day$, 1)>]
    let part1 fileName =
        fileName
        |> readText
        |> String.length
        |> printfn "%d"

    [<Solution($year$, $day$, 2)>]
    let part2 fileName =
        fileName
        |> readText
        |> String.length
        |> printfn "%d"