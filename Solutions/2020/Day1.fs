namespace Solutions2020

module Day1 = 

    open Common

    let rec combinations n l = 
        match n, l with
        | 0, _ -> [[]]
        | _, [] -> []
        | k, (x::xs) -> List.map ((@) [x]) (combinations (k-1) xs) @ combinations k xs
        
    [<Solution(2020, 1, 1)>]
    let part1 fileName =
        fileName
        |> readLinesAs int
        |> Seq.toList
        |> combinations 2
        |> Seq.find (fun p -> p |> Seq.sum = 2020)
        |> Seq.fold (*) 1

    [<Solution(2020, 1, 2)>]
    let part2 fileName =
        fileName
        |> readLinesAs int
        |> Seq.toList
        |> combinations 3
        |> Seq.find (fun p -> p |> Seq.sum = 2020)
        |> Seq.fold (*) 1
