namespace Solutions2025

module Day2 =
    open Common
    open System

    let productIds range =
        let parts = split "-" range
        let startRange = int64 parts.[0]
        let endRange = int64 parts.[1]
        seq {startRange..endRange}

    let allSame (items: seq<string>) =
        items
        |> Seq.pairwise
        |> Seq.forall (fun (a, b) -> a = b)
    
    let isInvalid (chunkCount: int) (productId: int64) =
        string productId 
            |> Seq.splitInto chunkCount
            |> Seq.map String
            |> allSame

    let anyChunkSizeInvalid (productId: int64) =
        let maxChunkSize = (string productId).Length
        seq{2..maxChunkSize}
        |> Seq.map (fun size -> isInvalid size productId) 
        |> Seq.exists (fun c -> c = true)


    [<Solution(2025, 2, 1)>]
    let part1 fileName =
        fileName
        |> readTextAs (split ",")
        |> Seq.collect productIds
        |> Seq.filter (isInvalid 2)
        |> Seq.sum

    [<Solution(2025, 2, 2)>]
    let part2 fileName =
        fileName
        |> readTextAs (split ",")
        |> Seq.collect productIds
        |> Seq.filter anyChunkSizeInvalid
        |> Seq.sum