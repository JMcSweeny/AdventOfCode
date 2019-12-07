namespace Solutions2019

module Day4 =
    open Common

    let parseInput fileName = 
        let line = 
            fileName
            |> readLines
            |> Seq.head
        let parts = line.Split('-')
        int parts.[0], int parts.[1]

    let doesNotDecrease = Seq.pairwise >> Seq.forall (fun (a, b) -> a <= b)
    let hasAPair = Seq.countBy id >> Seq.exists (fun (_, c) -> c >= 2)
    let hasSinglePair = Seq.countBy id >> Seq.exists(fun (_, c) -> c = 2)

    [<Solution(2019, 4, 1)>]
    let part1 fileName =
        let (rangeStart, rangeEnd) = parseInput fileName
        let allPasswords = seq {rangeStart..rangeEnd}

        allPasswords
        |> Seq.map string
        |> Seq.filter doesNotDecrease
        |> Seq.filter hasAPair
        |> Seq.length

    [<Solution(2019, 4, 2)>]
    let part2 fileName =
        let (rangeStart, rangeEnd) = parseInput fileName
        let allPasswords = seq {rangeStart..rangeEnd}

        allPasswords
        |> Seq.map string
        |> Seq.filter doesNotDecrease
        |> Seq.filter hasSinglePair
        |> Seq.length

    
