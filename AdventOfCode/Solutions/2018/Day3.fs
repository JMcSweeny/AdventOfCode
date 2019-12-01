namespace Solutions2018

module Day3 =
    open Common
    open Coordinates

    type Claim = { id: int; left: int; top: int; width: int; height: int }

    let parseClaim (line: string) =
        let parts = line.Split(" ,x:#".ToCharArray())
        { id = int parts.[1]; left = int parts.[3]; top = int parts.[4]; width = int parts.[6]; height = int parts.[7] }

    let parseInput fileName = 
        readLinesAs parseClaim fileName

    let getBoundries (claim: Claim) =
        { startX = claim.left; endX = claim.left + claim.width - 1; startY = claim.top; endY = claim.top + claim.height - 1 }

    let getCoords = getBoundries >> boundryToCoords >> Set.ofSeq

    [<Solution(2018, 3, 1)>]
    let part1 fileName =
        fileName
        |> parseInput
        |> Seq.collect getCoords
        |> Seq.countBy id
        |> Seq.filter (fun (_, count) -> count > 1)
        |> Seq.length

    [<Solution(2018, 3, 2)>]
    let part2 fileName =
        let input = parseInput fileName

        let nonIntersectedCoords = 
            input
            |> Seq.collect getCoords
            |> Seq.countBy id
            |> Seq.filter (fun (_, count) -> count = 1)
            |> Seq.map fst
            |> Set.ofSeq

        let doesNotIntersect (claim: Claim) =
            claim
            |> getCoords
            |> Seq.forall (fun coord -> Set.contains coord nonIntersectedCoords)

        input
        |> Seq.find doesNotIntersect
        |> fun claim -> claim.id