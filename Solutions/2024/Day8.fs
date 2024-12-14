namespace Soltions2024

module Day8 =
    open Common

    let buildAntennaMap (map: Map<char, list<int*int>>) (antenna: char, coord: int * int) =
        match antenna with
        | '.' -> map
        | _ -> Map.change antenna (fun v ->
                    match v with
                    | Some v -> Some (coord :: v)
                    | None -> Some [coord]
                ) map

    let getAntennaPairs (pairs: list<int * int>) =
        pairs
        |> List.indexed
        |> List.collect (fun (i, x) -> 
            pairs
            |> List.skip (i + 1)
            |> List.map (fun y -> (x, y)))

    let getAllAntiNodes (antenna1: int * int, antenna2: int * int) =
        List.unfold (fun ((a, b), (x, y)) -> 
            match (a + (a - x), b + (b - y)) with
            | (i, j) when i < 0 || i >= 50 || j < 0 || j >= 50 -> None
            | (i, j) -> Some ((i, j), ((i, j), (a, b)))
        ) (antenna1, antenna2)

    let getAntiNodes fn (antenna1, antenna2) =
        let firstAntiNodes = getAllAntiNodes (antenna1, antenna2) |> fn (antenna1, antenna2)
        let secondAntiNodes = getAllAntiNodes (antenna2, antenna1) |> fn (antenna1, antenna2)
        firstAntiNodes @ secondAntiNodes

    let getFirstAntiNode _ (antinodes: list<int * int>) =
       match antinodes with
       | a when a.Length = 0 -> []
       | a -> [List.head a]

    let appendAntennas (antenna1, antenna2) (antinodes: list<int * int>) =
        antenna1 :: antenna2 :: antinodes

    [<Solution(2024, 8, 1)>]
    let part1 fileName =
        fileName
        |> readLines
        |> Seq.mapi (fun x line -> line |> Seq.mapi (fun y c -> (c, (x, y))))
        |> Seq.collect id
        |> Seq.fold buildAntennaMap Map.empty
        |> Map.map (fun _ v -> getAntennaPairs v)
        |> Map.values
        |> Seq.collect id
        |> Seq.map (getAntiNodes getFirstAntiNode)
        |> Seq.collect id
        |> Seq.distinct
        |> Seq.length

    [<Solution(2024, 8, 2)>]
    let part2 fileName =
        fileName
        |> readLines
        |> Seq.mapi (fun x line -> line |> Seq.mapi (fun y c -> (c, (x, y))))
        |> Seq.collect id
        |> Seq.fold buildAntennaMap Map.empty
        |> Map.map (fun _ v -> getAntennaPairs v)
        |> Map.values
        |> Seq.collect id
        |> Seq.map (getAntiNodes appendAntennas)
        |> Seq.collect id
        |> Seq.distinct
        |> Seq.length




