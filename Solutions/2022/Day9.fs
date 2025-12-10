namespace Solutions2022

module Day9 =
    open Common

    type Coordinate = int * int

    let parseLine = split " " >> (fun parts -> Seq.init (parts.[1] |> int) (fun _ -> parts.[0]))

    let nextMove direction =
        match direction with
        | "R" -> 1, 0
        | "L" -> -1, 0
        | "U" -> 0, 1
        | "D" -> 0, -1
        | _ -> failwithf "Invalid Action %s" direction

    let getDistance (head: Coordinate) (tail: Coordinate) =
        (fst head - fst tail, snd head - snd tail)

    let moveLateral (headX, headY) (tailX, tailY) =
        let neighboursHead = Set.ofList [(headX-1,headY); (headX+1,headY); (headX,headY-1); (headX,headY+1)]
        let neighboursTail = Set.ofList [(tailX-1,tailY); (tailX+1,tailY); (tailX,tailY-1); (tailX,tailY+1)]    
        Set.intersect neighboursHead neighboursTail |> Set.maxElement

    let moveDiagonal (headX, headY) (tailX, tailY) =
        let neighboursHead = Set.ofList [(headX-1,headY); (headX+1,headY); (headX,headY-1); (headX,headY+1);
                                         (headX-1,headY-1); (headX-1,headY+1); (headX+1,headY-1); (headX+1,headY+1)]
        let neighboursTail = Set.ofList [(tailX-1,tailY-1); (tailX-1,tailY+1); (tailX+1,tailY-1); (tailX+1,tailY+1)]
        Set.intersect neighboursHead neighboursTail |> Set.maxElement


    let moveRope ((head: Coordinate), (tails: list<Coordinate>), (positions: Set<Coordinate>)) (direction: string) =
        let headX, headY = head
        let nextHeadX, nextHeadY = nextMove direction
        let nextHead = headX + nextHeadX, headY + nextHeadY

        let moveTails (currHead: Coordinate, nextTails: list<Coordinate>) (tail: Coordinate) = 
            let dx, dy = getDistance currHead tail

            if abs dx < 2 && abs dy < 2 then
                (tail, tail :: nextTails)
            elif (currHead |> fst) = (tail |> fst) || (currHead |> snd) = (tail |> snd) then
                let nextTail = moveLateral currHead tail
                (nextTail, nextTail :: nextTails)
            else
                let nextTail = moveDiagonal currHead tail
                (nextTail, nextTail :: nextTails)

        let _, nextTails = Seq.fold moveTails (nextHead, List.empty) tails

        (nextHead, nextTails |> List.rev, Set.add (nextTails |> List.head) positions)

    [<Solution(2022, 9, 1)>]
    let part1 fileName =
        fileName
        |> readLinesAs parseLine
        |> Seq.concat
        |> Seq.fold moveRope ((0, 0), [(0, 0)], [(0,0)] |> Set.ofSeq)
        |> (fun (_, _, positions) -> positions)
        |> Set.count

    [<Solution(2022, 9, 2)>]
    let part2 fileName =
        fileName
        |> readLinesAs parseLine
        |> Seq.concat
        |> Seq.fold moveRope ((0, 0), [(0, 0); (0, 0); (0, 0); (0, 0); (0, 0); (0, 0); (0, 0); (0, 0); (0, 0)], [(0,0)] |> Set.ofSeq)
        |> (fun (_, _, positions) -> positions)
        |> Set.count
