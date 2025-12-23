namespace Solutions2025

module Day5 =
    open Common

    let parseRange (line: string) =
        match split "-" line with
        | [| startRange; endRange |] -> int64 startRange, int64 endRange
        | _ -> failwith "Invalid range format"

    let gatherIngredients (freshIngredients: List<int64 * int64>, ingredients: List<int64>) (line: string)  =
        match line with
        | "" -> freshIngredients, ingredients
        | l when l |> Seq.contains '-' ->
            freshIngredients @ [parseRange l], ingredients
        | _ -> 
            freshIngredients, ingredients @ [int64 line]
    
    let isFresh (freshIngredients: List<int64 * int64>) (ingredient: int64) =
        freshIngredients
        |> Seq.exists (fun (startRange, endRange) -> ingredient >= startRange && ingredient <= endRange)

    let mergeRanges (freshIngredients: List<int64 * int64>) =
        let rec merge (mergedRanges: List<int64 * int64>) (remaining: List<int64 * int64>) =
            match remaining with
            | [] -> mergedRanges
            | [head] -> mergedRanges @ [head]
            | head :: next :: tail ->
                let startHead, endHead = head
                let startNext, endNext = next
                let isOverlapping = startNext >= startHead && startNext <= endHead
                match isOverlapping with
                | false -> merge (mergedRanges @ [head]) ([next] @ tail)
                | true -> 
                    let newRange = startHead, max endHead endNext
                    merge mergedRanges ([newRange] @ tail)
        merge List.empty freshIngredients

    let countTotal (count: int64) (startRange: int64, endRange: int64) =
        count + (endRange - startRange) + int64 1

    [<Solution(2025, 5, 1)>]
    let part1 fileName =
        fileName
        |> readLines
        |> Seq.fold gatherIngredients (List.Empty, List.Empty)
        |> fun (freshIngredients, ingredients) -> 
            ingredients |> Seq.filter (fun i -> isFresh freshIngredients i)
        |> Seq.length

    [<Solution(2025, 5, 2)>]
    let part2 fileName =
        fileName
        |> readLines
        |> Seq.fold gatherIngredients (List.Empty, List.Empty)
        |> fst
        |> List.sortBy (fun (startRange, _) -> startRange)
        |> mergeRanges
        |> Seq.fold countTotal 0
