module Day5 where

import Utils 
import Data.List

data Range = Range { src :: Int, dst :: Int, range :: Int} deriving (Show)
data Conversion = Conversion { from :: String, to :: String, list :: [Range] } deriving (Show)
data Game = Game { seeds :: [Int], maps :: [Conversion] } deriving (Show)
data Memo = Memo { start :: Int, end :: Int, minVal :: Int} deriving (Show)

parseLine :: String -> Range
parseLine input = do
    let [dst, src, range] = split ' ' input
    Range{src = read src, dst = read dst, range = read range}

isMapLine :: String -> Bool
isMapLine = isSuffixOf "map:"

parseSeedLine :: String -> [Int]
parseSeedLine ('s':'e':'e':'d':'s':':':' ':rest) = map read (split ' ' rest)

parseMapLine :: String -> (String, String)
parseMapLine input = do
    let (src:rest) = split '-' input
    let (to:remaining) = rest
    let (dst:_) = split ' ' (head remaining)
    (src, dst)

parseMap :: [String] -> (Conversion, [String])
parseMap (mapLine: rest) = do
    let (from, to) = parseMapLine mapLine
    let (mapInput, remaining) = takeUntil rest isMapLine
   
    let seeds = map parseLine mapInput

    (Conversion{from = from, to = to, list = seeds}, remaining)
    
parseAllMaps :: [String] -> [Conversion]
parseAllMaps [] = []
parseAllMaps input = do
    let (conversion, remaining) = parseMap input
    conversion : parseAllMaps remaining

parseGame :: [String] -> Game
parseGame inputLines = do
    let (seedLine,rest) = takeUntil inputLines isMapLine
    let seeds = parseSeedLine (head seedLine)
    let maps = parseAllMaps rest
    Game{seeds = seeds, maps = maps}

findConversionWithSrc :: [Conversion] -> String -> (Conversion, [Conversion])
findConversionWithSrc (c@Conversion{from = from} : cs) target = if from == target
    then (c, cs)
    else do
        let (found, remaining) = findConversionWithSrc cs target
        (found, c : remaining)

findConversionRange :: Conversion -> Int -> Int
findConversionRange c@Conversion{from = from, to = to, list = (r:rs)} target = do
    let Range{src = src, dst = dst, range = range} = r
    if target >= src && target < src + range then do
        let diff = target - src 
        dst + diff
    else findConversionRange Conversion{from = from, to = to, list = rs} target
findConversionRange c@Conversion{from = from, to = to, list = []} target = target

untilLocation :: [Conversion] -> (String, Int) -> Int
untilLocation maps (currentName, currentVal) = do
    let (target, remaining) = findConversionWithSrc maps currentName
    let Conversion{from = nextFrom, to = nextTo} = target
    let nextValue = findConversionRange target currentVal

    if nextTo == "location"
        then nextValue
        else untilLocation remaining (nextTo, nextValue)

traverseEachSeed :: Game -> [Int] -> [Int]
traverseEachSeed Game{ seeds = (s:ss), maps = maps} accum = do
    let (start, remaining) = findConversionWithSrc maps "seed"
    let Conversion{to = nextTo} = start 
    let startVal = findConversionRange start s
    let val = untilLocation remaining (nextTo, startVal)
    traverseEachSeed Game{seeds = ss, maps = maps} (val : accum)

traverseEachSeed Game{ seeds = [], maps = maps} accum = accum

partOne :: Game -> Int
partOne g = do
    let seeds = traverseEachSeed g []
    minimum seeds

memoizedRange :: Int -> Int -> [Memo] -> Maybe Int
memoizedRange start end (Memo{start = mStart, end = mEnd, minVal = minVal} : ms) = do
    if start >= mStart && end <= mEnd then
        Just minVal
    else memoizedRange start end ms
memoizedRange _ _ [] = Nothing

splitRange :: Int -> Int -> Int -> [(Int, Int)]
splitRange start stop step = if start + stop < step then [(start, stop)]
    else do
        let newStart = start + step
        (start, newStart) : splitRange newStart stop step

-- WIP: did not finish updating this function
partTwoForRange :: Int -> Int -> [Conversion] -> [Memo] -> (Int, [Memo])
partTwoForRange start stop maps memo = do
    let memVal = memoizedRange start stop memo
    case memVal of
        Just n -> (n, memo)
        Nothing -> do
                let memoRange = 50000
                let diff = stop - start
                if diff > memoRange do
                    let ranges = splitRange start stop memoRange
                    let res = map (\r -> do
                            let (rageStart, rangeStop) = r
                            partTwoForRange rangeStart rangeStop maps memo) ranges
                    unzip res
                else minimum (traverseEachSeed Game{ seeds = [start..stop], maps = maps} [])

-- does not work yet :(
-- either needs better memoisation at path traversing level, more tail recursion or
-- different traversal implementation (e.g tree via range rather than for each individual point) 
partTwo :: Game -> [Memo] -> (Int, [Memo])
partTwo Game{ seeds = (s1:s2:ss), maps = maps} memo = do
    let memoRange = 50000
    if s2 <= memoRange
        then do
            let end = s1 + s2
            let leftVal = partTwoForRange s1 end maps memo
            let leftMemo = Memo{start = s1, end = end, minVal = leftVal}

            let (rightVal, rightMemo) = partTwo Game{ seeds = ss, maps = maps} (leftMemo : memo)
            let minVal = min leftVal rightVal
            (minVal, rightMemo) 
    else do
        let (leftVal, leftMemo) = partTwo Game { seeds = [s1,s2-memoRange], maps = maps } memo
        let topMemo = Memo{start = s1, end = s1+s2-2000, minVal = leftVal}

        let rightRange = s2-memoRange
        let rightStart = s1+memoRange
        let rightEnd = rightStart + rightRange

        let (rightVal, rightMemo) = partTwo Game { seeds = rightStart:rightRange:ss, maps = maps} (topMemo : leftMemo)
        let minVal = min leftVal rightVal
        let newMemo  = Memo{start = rightStart, end = rightEnd, minVal = minVal}
        (minVal, newMemo : rightMemo)

partTwo Game{ seeds = [], maps = _} memo = (maxBound, memo)


main = do
    input <- readFile "input.txt"
    let inputLines = filter (not.null) (lines input)
    let game = parseGame inputLines
    let p1 = partOne game
    let (p2, _) = partTwo game []
    print "Part 1\n"
    print p1
    print "Part 2\n"
    print p2
