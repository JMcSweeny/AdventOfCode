namespace Soltions2024

module Day4 =
    open Common
    open System

    let matchesAtPoint (grid: list<list<char>>) (x: int) (y: int) =
        let hasNorth =
            match y with
            | y when y < 3 -> 0
            | _ when String.Concat([grid.[x][y]; grid.[x][y - 1]; grid.[x][y - 2]; grid.[x][y - 3]]) = "XMAS" -> 1
            | _ -> 0

        let hasNorthEast =
            match x, y with
            | x, y when y < 3 || x > grid.[0].Length - 4 -> 0
            | _ when String.Concat([grid.[x][y]; grid.[x + 1][y - 1]; grid.[x + 2][y - 2]; grid.[x + 3][y - 3]]) = "XMAS" -> 1
            | _ -> 0

        let hasEast =
            match x with
            | x when x > grid.[0].Length - 4 -> 0
            | _ when String.Concat([grid.[x][y]; grid.[x + 1][y]; grid.[x + 2][y]; grid.[x + 3][y]]) = "XMAS" -> 1
            | _ -> 0

        let hasSouthEast =
            match x, y with
            | x, y when y > grid.Length - 4 || x > grid.[0].Length - 4 -> 0
            | _ when String.Concat([grid.[x][y]; grid.[x + 1][y + 1]; grid.[x + 2][y + 2]; grid.[x + 3][y + 3]]) = "XMAS" -> 1
            | _ -> 0

        let hasSouth =
            match y with
            | y when y > grid.Length - 4 -> 0
            | _ when String.Concat([grid.[x][y]; grid.[x][y + 1]; grid.[x][y + 2]; grid.[x][y + 3]]) = "XMAS" -> 1
            | _ -> 0

        let hasSouthWest =
            match x, y with
            | x, y when x < 3 || y > grid.Length - 4 -> 0
            | _ when String.Concat([grid.[x][y]; grid.[x - 1][y + 1]; grid.[x - 2][y + 2]; grid.[x - 3][y + 3]]) = "XMAS" -> 1
            | _ -> 0

        let hasWest =
            match x with
            | x when x < 3 -> 0
            | _ when String.Concat([grid.[x][y]; grid.[x - 1][y]; grid.[x - 2][y]; grid.[x - 3][y]]) = "XMAS" -> 1
            | _ -> 0

        let hasNorthWest =
            match x, y with
            | x, y when x < 3 || y < 3 -> 0
            | _ when String.Concat([grid.[x][y]; grid.[x - 1][y - 1]; grid.[x - 2][y - 2]; grid.[x - 3][y - 3]]) = "XMAS" -> 1
            | _ -> 0

        hasNorth + 
        hasNorthEast + 
        hasEast + 
        hasSouthEast + 
        hasSouth + 
        hasSouthWest + 
        hasWest +
        hasNorthWest

    let findMatchesInGrid fn (grid: list<list<char>>) =
        grid
        |> Seq.mapi (fun x line -> line |> Seq.mapi (fun y _ -> fn grid x y ))
        |> Seq.sumBy (fun outer -> outer |> Seq.sum)

    let crossMatchesAtPoint (grid: list<list<char>>) (x: int) (y: int) =
        let getTop a b = String.Concat([grid.[a - 1][b - 1]; grid.[a][b]; grid.[a + 1][b + 1]])
        let getBottom a b = String.Concat([grid.[a - 1][b + 1]; grid.[a][b]; grid.[a + 1][b - 1]])
            
        match x, y with
        | x, y when grid.[x][y] <> 'A' -> 0
        | x, y when x < 1 || x > grid.[0].Length - 2  || y < 1 || y > grid.Length - 2 -> 0
        | x, y when (getTop x y = "MAS" || getTop x y = "SAM") && (getBottom x y = "MAS" || getBottom x y = "SAM") -> 1
        | _ -> 0

    [<Solution(2024, 4, 1)>]
    let part1 fileName =
        fileName
        |> readLinesAs Seq.toList
        |> Seq.toList
        |> findMatchesInGrid matchesAtPoint

    [<Solution(2024, 4, 2)>]
    let part2 fileName =
        fileName
        |> readLinesAs Seq.toList
        |> Seq.toList
        |> findMatchesInGrid crossMatchesAtPoint