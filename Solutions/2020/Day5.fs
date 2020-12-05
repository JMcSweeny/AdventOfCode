namespace Soltions2020

module Day5 = 
    open Common

    let initialRowRange = seq { 0..127 }
    let initialColumnRange = seq { 0..7 }

    let parseInstruction lowerChar upperChar initialRange =
        let foldInstruction range c =
            let mid = (range |> Seq.length) / 2
            match c with
            | _ when c = lowerChar -> range |> Seq.take mid
            | _ when c = upperChar -> range |> Seq.skip mid
            | _ -> range
        Seq.fold foldInstruction initialRange >> Seq.head 

    let parseRow = Seq.take 7 >> parseInstruction 'F' 'B' initialRowRange
    let parseColumn = Seq.skip 7 >> parseInstruction 'L' 'R' initialColumnRange

    let parseSeatID (s: string) =
        let row = s |> parseRow
        let column = s |> parseColumn
        row * 8 + column
        
    [<Solution(2020, 5, 1)>]
    let part1 fileName =
        fileName 
        |> readLines
        |> Seq.map parseSeatID
        |> Seq.max
        
    [<Solution(2020, 5, 2)>]
    let part2 fileName =
        fileName 
        |> readLines
        |> Seq.map parseSeatID
        |> Seq.sort
        |> Seq.pairwise
        |> Seq.find (fun (i, j) -> j - i <> 1)
        |> fst
        |> (+) 1




