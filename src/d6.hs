module Day6 where

import Utils

waysToWin :: Int -> Int -> Int
waysToWin maxTime minDistance = do
    let maxSpeed = maxTime - 1
    let minWinSpeed = (minDistance `div` maxTime) - 1 

    let timeLeft = maxTime - minWinSpeed

    let speeds = [minWinSpeed..maxSpeed]

    sum (zipWith (curry (\si -> do
                let (i, s) = si
                let t = timeLeft - i
                if t * s > minDistance then 1 else 0
            )) [0..] speeds)

parseLinePartOne :: String -> [Int]
parseLinePartOne input = do
    let (_:splits) = split ':' input
    let eachNum = split ' ' (head splits)
    map read (filter (not.null) eachNum)

partOne :: [(Int, Int)] -> Int
partOne [] = 1
partOne ((t, d):games) = do
    let results = waysToWin t d
    results * partOne games

parseLinePartTwo :: String -> Int
parseLinePartTwo input = do
    let (_:splits) = split ':' input
    let eachNum = concat (split ' ' (head splits))
    read eachNum

main = do
    input <- readFile "input.txt"
    let [timeLine, distanceLine] = lines input
    let parsedGamesForOne = zip (parseLinePartOne timeLine) (parseLinePartOne distanceLine)
    let p1 = partOne parsedGamesForOne
    print "Part1: \n"
    print p1
    let t2 = parseLinePartTwo timeLine
    let d2 = parseLinePartTwo distanceLine
    let p2 = partOne [(t2, d2)]
    print "Part2: \n"
    print p2


