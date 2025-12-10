namespace Solutions2023

module Day2 =
    open Common

    type Game = {
        Id: int;
        Red: int;
        Green: int;
        Blue: int;
    }

    let parseRoll game cube =
        let cubeParts = split " " cube
        let count = int cubeParts.[0]
        let color = cubeParts.[1]

        match color with
        | c when c = "red" && count > game.Red -> { game with Red = count}
        | c when c = "green" && count > game.Green -> { game with Green = count }
        | c when c = "blue" && count > game.Blue -> { game with Blue = count }
        | _ -> game

    let parseInput line =
        let gameParts = split ": " line
        let idParts = split " " gameParts.[0]
        let rollParts = Seq.collect (split ", ") (split "; " gameParts.[1])

        let id = int idParts.[1]
        Seq.fold parseRoll { Id = id; Red = 1; Green = 1; Blue = 1; } rollParts

    let isValidGame game =
        game.Red <= 12 &&
        game.Green <= 13 &&
        game.Blue <= 14

    let getPower game =
        game.Red * 
        game.Green * 
        game.Blue


    [<Solution(2023, 2, 1)>]
    let part1 fileName =
        fileName
        |> readLinesAs parseInput
        |> Seq.filter isValidGame
        |> Seq.sumBy (fun game -> game.Id)

    [<Solution(2023, 2, 2)>]
    let part2 fileName =
        fileName
        |> readLinesAs parseInput
        |> Seq.sumBy getPower
