namespace Soltions2022

module Day8 =
    open Common

    let parseLine = Seq.toList >> List.map string >> List.map int

    let getTreeInRow (trees: list<list<int>>) (x: int) (y: int) = ((x, y), trees.[x].[y])
    let getTreeInColumn (trees: list<list<int>>) (x: int) (y: int) = ((y, x), trees.[y].[x])
    
    let findVisible (getTree) (max) (outer: int) (inner: seq<int>) =
        let isEdge (x, y) = x = 0 || x = max || y = 0 || y = max
        inner
        |> Seq.fold (fun ((visible: Set<int * int>), (tallest: int)) i -> 
            let (coord, tree) = getTree outer i
            match tallest with
            | _ when isEdge coord -> ((visible |> Set.add coord), tree)
            | t when tree > t -> ((visible |> Set.add coord), tree)
            | _ -> (visible, tallest)
        ) (Set.empty, 0)
        |> fst

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

    let getScenicScore (getTree) (max) (outer: int) (inner: seq<int>) =
        inner
        |> Seq.fold (fun ((scores: Map<int * int, int>), (treeLine: list<int>)) i ->
            let (coord, tree) = getTree outer i
            let treesInView = getTreesInView treeLine tree
            (scores |> Map.add coord treesInView, tree :: treeLine)
        ) (Map.empty, List.empty)
        |> fst

    let traverseTrees fn (trees: list<list<int>>) =
        let max = (trees |> Seq.length) - 1
        let perimeter = [0..max]

        let rowFn = fn (getTreeInRow trees) max
        let columnFn = fn (getTreeInColumn trees) max

        perimeter
        |> Seq.collect (fun num ->
            let rowFoward = rowFn num perimeter
            let rowBackward = rowFn num (perimeter |> List.rev)
            let columnForward = columnFn num perimeter
            let columnBackward = columnFn num (perimeter |> List.rev)

            [| rowFoward; rowBackward; columnForward; columnBackward |]
        )

    [<Solution(2022, 8, 1)>]
    let part1 fileName =
        fileName
        |> readLinesAs parseLine
        |> List.ofSeq
        |> traverseTrees findVisible
        |> Set.unionMany
        |> Set.count

    [<Solution(2022, 8, 2)>]
    let part2 fileName =
        fileName
        |> readLinesAs parseLine
        |> List.ofSeq
        |> traverseTrees getScenicScore
        |> mergeScores
        |> Map.toSeq
        |> Seq.map snd
        |> Seq.max
