namespace Soltions2022

module Day8 =
    open Common

    let parseLine = Seq.toList >> List.map string >> List.map int

    let findAllVisible (trees: list<list<int>>) =
        let max = (trees |> Seq.length) - 1
        let perimeter = [0..max]

        let isEdge (x, y) = x = 0 || x = max || y = 0 || y = max

        let getTreeInRow (x: int) (y: int) = ((x, y), trees.[x].[y])
        let getTreeInColumn (x: int) (y: int) = ((y, x), trees.[y].[x])

        
        let findVisible (getTree) (outer: int) (inner: seq<int>) =
            inner
            |> Seq.fold (fun ((visible: Set<int * int>), (tallest: int)) i -> 
                let (coord, tree) = getTree outer i
                match tallest with
                | _ when isEdge coord -> ((visible |> Set.add coord), tree)
                | t when tree > t -> ((visible |> Set.add coord), tree)
                | _ -> (visible, tallest)
            ) (Set.empty, 0)
            |> fst

        let findVisibleInRow = findVisible getTreeInRow
        let findVisibleInColumn = findVisible getTreeInColumn

        perimeter
        |> Seq.collect (fun num ->
            let visibleInRowForward = findVisibleInRow num perimeter
            let visibleInRowBackward = findVisibleInRow num (perimeter |> List.rev)
            let visibleInColumnForward = findVisibleInColumn num perimeter
            let visibleInColumnBackward = findVisibleInColumn num (perimeter |> List.rev)

            [| visibleInRowForward; visibleInRowBackward; visibleInColumnForward; visibleInColumnBackward |]
        )

    let mergeScores (scoreList: seq<Map<int * int, int>>) =
        scoreList
        |> Seq.fold (fun allScores scores -> 
            scores
            |> Map.fold (fun acc key value -> 
                Map.change key (fun v -> 
                  match v with
                  | Some x when x = 0 -> Some (x * 1)
                  | Some x -> Some (x * value)
                  | None -> Some value
                ) acc
            ) allScores
        ) Map.empty

    let getTreesInView treeLine tree =
        let rec loop trees count =
            match trees with
            | [] -> count
            | h::t when h < tree -> loop t (count + 1)
            | _ -> count + 1

        loop treeLine 0

    let getScenicScores (trees: list<list<int>>) =
        let max = (trees |> Seq.length) - 1
        let perimeter = [0..max]

        let getTreeInRow (x: int) (y: int) = ((x, y), trees.[x].[y])
        let getTreeInColumn (x: int) (y: int) = ((y, x), trees.[y].[x])

        let getScenicScore (getTree) (outer: int) (inner: seq<int>) =
            inner
            |> Seq.fold (fun ((scores: Map<int * int, int>), (treeLine: list<int>)) i ->
                let (coord, tree) = getTree outer i
                let treesInView = getTreesInView treeLine tree
                (scores |> Map.add coord treesInView, tree :: treeLine)
            ) (Map.empty, List.empty)
            |> fst

        let getScenicScoresInRow = getScenicScore getTreeInRow
        let getScenicScoresInColumn = getScenicScore getTreeInColumn

        perimeter
        |> Seq.collect (fun num ->
            let scenicScoresInRowForward = getScenicScoresInRow num perimeter
            let scenicScoresInRowBackward = getScenicScoresInRow num (perimeter |> List.rev)
            let scenicScoresInColumnForward = getScenicScoresInColumn num perimeter
            let scenicScoresInColumnBackward = getScenicScoresInColumn num (perimeter |> List.rev)

            [| scenicScoresInRowForward; scenicScoresInRowBackward; scenicScoresInColumnForward; scenicScoresInColumnBackward |]
        )

    [<Solution(2022, 8, 1)>]
    let part1 fileName =
        fileName
        |> readLinesAs parseLine
        |> List.ofSeq
        |> findAllVisible
        |> Set.unionMany
        |> Set.count

    [<Solution(2022, 8, 2)>]
    let part2 fileName =
        fileName
        |> readLinesAs parseLine
        |> List.ofSeq
        |> getScenicScores
        |> mergeScores
        |> Map.toSeq
        |> Seq.map snd
        |> Seq.max
