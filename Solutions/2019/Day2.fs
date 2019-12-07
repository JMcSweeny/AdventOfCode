namespace Solutions2019

module Day2 =
    open Common

    let parseInput fileName =
        fileName
        |> readLines
        |> Seq.collect (fun l -> l.Split(','))
        |> Seq.map int

    let replaceAtIndex index value sequence =
        Seq.mapi (fun i v -> if i = index then value else v) sequence

    let scanOptcode (intcode, _) index =
        let getCodeAtIndex i = Seq.item i intcode
        let optcode = getCodeAtIndex index
        match optcode with
            | 99 -> (intcode, optcode)
            | _ -> 
                let firstIndex = getCodeAtIndex (index + 1)
                let secondIndex = getCodeAtIndex (index + 2)
                let thirdIndex = getCodeAtIndex (index + 3)
                let firstValue = getCodeAtIndex firstIndex
                let secondValue = getCodeAtIndex secondIndex
                let f = if optcode = 1 then (+) else (*)
                let result = f firstValue secondValue
                replaceAtIndex thirdIndex result intcode, optcode

    let compute noun verb initialIntcode =
        let intcode = 
            initialIntcode
            |> replaceAtIndex 1 noun
            |> replaceAtIndex 2 verb

        Seq.unfold (fun i -> Some(i, i + 4)) 0
        |> Seq.scan scanOptcode (intcode, 0)
        |> Seq.takeWhile (fun (_, optcode) -> optcode <> 99)
        |> Seq.last
        |> fst
        |> Seq.head

    [<Solution(2019, 2, 1)>]
    let part1 fileName =
        fileName
        |> parseInput
        |> compute 12 2

    [<Solution(2019, 2, 2)>]
    let part2 fileName = 
        let initialIntcode = parseInput fileName
        let allNouns = seq {0..99}
        let allVerbs = seq {0..99}
        let allCombos = allNouns |> Seq.collect (fun n -> allVerbs |> Seq.map (fun v -> (n, v)))

        let computeCombo (_: int, _: int) (noun: int, verb: int) =
            let result = compute noun verb initialIntcode
            let product = 100 * noun + verb
            (result, product)

        allCombos
        |> Seq.scan computeCombo (0, 0)
        |> Seq.find (fun (result, _) -> result = 19690720)
        |> snd






        

