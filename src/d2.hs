module Day2 where

import Prelude
import Control.Exception (evaluate)

data Colour = Red | Green | Blue deriving (Show)
data Move = Move { colour :: Colour, value :: Int } deriving (Show)

type Subset = [Move]
data Game = Game { identifier :: Int, moveSets :: [Subset]} deriving (Show)
type Restriction = [Move]

trimLeading :: String -> String
trimLeading (' ' : xs) = trimLeading xs
trimLeading xs = xs

_split :: Char -> String -> String -> [String]
_split c (x : xs) accum = if c == x
    then accum : _split c xs []
    else _split c xs (accum ++ [x])

_split _ [] accum = [accum]

split :: Char -> String -> [String]
split c xs = _split c xs []

parseGameId :: String -> Int
parseGameId input = do
    let [_, numVal] = split ' ' input
    read numVal

_parseMove :: String -> String -> Move
_parseMove numVal "red" = Move{colour = Red, value = read numVal}
_parseMove numVal "green" = Move{colour = Green, value = read numVal}
_parseMove numVal "blue" = Move{colour = Blue, value = read numVal}

parseMoveStr :: String -> Move
parseMoveStr input = do
    let [numVal, colour] = split ' ' input
    _parseMove numVal colour

parseSubsetStr :: String -> Subset
parseSubsetStr input = do
    let moves = split ',' input
    map (parseMoveStr.trimLeading) moves

parseGame :: String -> Game
parseGame input = do
    let [gameIdUnparsed, rest] = split ':' input
    let gameId = parseGameId gameIdUnparsed
    
    let subsets = split ';' rest

    let subsetsParsed = map parseSubsetStr subsets

    Game{identifier = gameId, moveSets = subsetsParsed}

getRestrictionFor :: Restriction -> Colour -> Int
getRestrictionFor (Move{colour = Red, value = x} : ms) Red = x
getRestrictionFor (Move{colour = Green, value = x} : ms) Green = x
getRestrictionFor (Move{colour = Blue, value = x} : ms) Blue = x
getRestrictionFor (m : ms) c = getRestrictionFor ms c

moveSetValid :: Subset -> Restriction -> Bool
moveSetValid (Move{colour = c, value = v} : ms) r = (v <= getRestrictionFor r c) && moveSetValid ms r
moveSetValid [] _ = True

partOne :: [Game] -> Int
partOne (Game{identifier = id, moveSets = subsets} : gs) = do
    let r = [Move{colour = Red, value = 12}, Move{colour = Green, value = 13}, Move{colour = Blue, value = 14}]
    let evaluatedSubsets = map (`moveSetValid` r) subsets
    let allSubsetsValid = and evaluatedSubsets
    if allSubsetsValid then id + partOne gs else partOne gs 
partOne [] = 0

replaceInRestrction :: Restriction -> Colour -> Int -> Restriction
replaceInRestrction (Move{colour = Red, value = x}: ms) Red v = Move{colour = Red, value = v} : ms
replaceInRestrction (Move{colour = Green, value = x}: ms) Green v = Move{colour = Green, value = v} : ms
replaceInRestrction (Move{colour = Blue, value = x}: ms) Blue v = Move{colour = Blue, value = v} : ms
replaceInRestrction (m:ms) c v = m : replaceInRestrction ms c v

_minReduce :: Subset -> Restriction -> Restriction
_minReduce (Move{colour = c, value = x} : ms) r = do
    let curMin = getRestrictionFor r c
    let newRestriction = if x > curMin
        then replaceInRestrction r c x
        else r
    _minReduce ms newRestriction
_minReduce [] r = r

minReduce :: Subset -> Restriction
minReduce moves = _minReduce moves [Move{colour = Red, value = 0}, Move{colour = Green, value = 0}, Move{colour = Blue, value = 0}]

getSetValues :: [Move] -> [Int]
getSetValues (Move{value = v} : ms) = v : getSetValues ms
getSetValues [] = []

partTwoForGame :: Game -> Int
partTwoForGame Game{moveSets = subsets} = do
    let minRestrictions = map minReduce subsets
    let restrictionsForGame = minReduce (concat minRestrictions)
    let values = getSetValues restrictionsForGame
    product values

partTwo :: [Game] -> Int
partTwo games = do
    let gameSets = map partTwoForGame games
    sum gameSets 

main = do
    input <- readFile "input.txt"
    let inputLines = lines input
    let games = map parseGame inputLines
    let p1 = partOne games
    let p2 = partTwo games
    print "Part 1: \n"
    print p1
    print "Part 2: \n"
    print p2
