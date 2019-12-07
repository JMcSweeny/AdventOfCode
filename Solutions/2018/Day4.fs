namespace Solutions2018

module Day4 =
    open Common

    type Action = 
        | GuardShiftStart of int
        | FallAsleep
        | WakeUp

    type Log = { dateTime: System.DateTime; action: Action }

    let parseLog (log: string) =
        let parts = log.Split(' ')
        let date = System.DateTime.Parse(log.Substring(1, 16))
        let action = 
            match parts.[2] with
            | "Guard" -> GuardShiftStart (parts.[3].Substring(1, parts.[3].Length - 1) |> int)
            | "falls" -> FallAsleep
            | "wakes" -> WakeUp
            | _ -> failwithf "Invalid Action %s" parts.[2]
        { dateTime = date; action = action }

    let parseInput = readLinesAs parseLog >> Seq.sortBy (fun l -> l.dateTime)

    let calculateLog (currentGuard: int, minuteAsleep: int, sleepRecord: Map<int, int list>) (log: Log) =
        match log.action with
        | GuardShiftStart guardId -> (guardId, minuteAsleep, sleepRecord)
        | FallAsleep -> (currentGuard, log.dateTime.Minute, sleepRecord)
        | WakeUp ->
            let sleepList = [minuteAsleep..log.dateTime.Minute - 1]
            let existingSleep =
                match sleepRecord.TryFind currentGuard with
                | Some record -> record
                | None -> []
            let newList = List.append existingSleep sleepList
            (currentGuard, minuteAsleep, Map.add currentGuard newList sleepRecord)

    let calulcateSleep = Seq.fold calculateLog (0, 0, Map.empty) >> fun (_, _, sleepRecord) -> sleepRecord

    let findMostAsleep = Map.toSeq >> Seq.maxBy (snd >> List.length) >> fst
    let findMinuteMostAsleep = Seq.countBy id >> Seq.maxBy snd

    let findMostAsleepAtSingleMinute = Map.map (fun _ v -> findMinuteMostAsleep v) >> Map.toSeq >> Seq.maxBy (snd >> snd)

    [<Solution(2018, 4, 1)>]
    let part1 fileName = 
        let logs = parseInput fileName
        let sleepRecord = logs |> calulcateSleep
        let mostAsleep = findMostAsleep sleepRecord
        let minuteMostAsleep = Map.find mostAsleep sleepRecord |> (findMinuteMostAsleep >> fst)
        string (mostAsleep * minuteMostAsleep)

    [<Solution(2018, 4, 2)>]
    let part2 fileName =
        fileName
        |> parseInput
        |> calulcateSleep
        |> findMostAsleepAtSingleMinute
        |> fun (id, (min, _)) -> id * min
        |> string