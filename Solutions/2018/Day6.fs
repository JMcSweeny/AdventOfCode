namespace Solutions2018

module Day6 = 
    open Common
    open Coordinates

    let parseLineAsCoord (line: string) = 
        let parts = line.Split ", "
        int parts.[0], int parts.[1]

    let getBoundries (coords: Set<int * int>) =
        let xVals = coords |> Set.map fst
        let yVals = coords |> Set.map snd
        { startX = Seq.min xVals; endX = Seq.max xVals; startY = Seq.min yVals; endY = Seq.max yVals }

    let isCoord (x, y) coords = Set.contains (x, y) coords

    let isBoundry (x, y) boundries = x = boundries.startX || x = boundries.endY || y = boundries.startY || y = boundries.endY

    let foldDistance (beacons: Set<int * int>) (coord: int * int) (distanceMap: Map<int * int, (int * int) list>) =
        let distances = beacons |> Set.map (fun c -> (c, getManhattanDistance c coord))
        let minDistance = distances |> Seq.minBy snd |> snd

        let minCoords = distances |> Seq.filter (fun (_, d) -> d = minDistance) |> Seq.toList

        let upsertDistanceMap k valToAdd =
            let newCoords = 
                match distanceMap.TryFind k with
                    | Some coordList -> valToAdd::coordList
                    | None -> [valToAdd]
            Map.add k newCoords distanceMap

        match minCoords with
            | x::_ when minCoords.Length = 1 -> upsertDistanceMap (fst x) coord
            | x::_s when (x |> snd) = 0 -> distanceMap 
            | _ -> distanceMap

    let getDistanceMap beacons allCoords = allCoords |> Set.fold (fun map c -> foldDistance beacons c map) Map.empty

    let isInifinite boundries coords = coords |> List.exists (fun c -> isBoundry c boundries)

    let filterInfinite boundries distanceMap  = 
        distanceMap |> Map.filter (fun _ coords -> not (isInifinite boundries coords))

    let foldTotalDistance (beacons: Set<int * int>) (coord: int * int) (totalDistanceMap: Map<int * int, int>) =
        let totalDistance = beacons |> Seq.sumBy (fun c -> getManhattanDistance c coord)
        Map.add coord totalDistance totalDistanceMap

    let getTotalDistanceMap beacons allCoords = allCoords |> Set.fold (fun map c -> foldTotalDistance beacons c map) Map.empty

    [<Solution(2018, 6, 1)>]
    let part1 fileName = 
        let beacons = readLinesAs parseLineAsCoord fileName |> Set.ofSeq
        let boundries = getBoundries beacons
        let allCoords = boundryToCoords boundries |> Set.ofSeq
        let distanceMap = getDistanceMap beacons allCoords

        distanceMap
        |> filterInfinite boundries
        |> Map.toSeq
        |> Seq.maxBy (fun (_, v) -> v.Length)
        |> snd
        |> List.length


    [<Solution(2018, 6, 2)>]
     let part2 fileName = 
        let beacons = readLinesAs parseLineAsCoord fileName |> Set.ofSeq
        let boundries = getBoundries beacons
        let allCoords = boundryToCoords boundries |> Set.ofSeq

        let totalDistanceMap = getTotalDistanceMap beacons allCoords

        totalDistanceMap
        |> Map.filter (fun _ v -> v < 10000)
        |> Map.toSeq
        |> Seq.length

