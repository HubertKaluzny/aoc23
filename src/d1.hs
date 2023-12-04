module Day1 where
import Data.Char

pickCalibrationValues :: [Char] -> Int
pickCalibrationValues [a, b] = read [a, b]
pickCalibrationValues [a] = read [a, a]
pickCalibrationValues input = read [head input, last input]

valuesInP1 :: String -> [Char]
valuesInP1 input = do
    filter (\y -> y >= '1' && y <= '9') input

partOne :: [String] -> Int
partOne input = do
    let calibrationValues = map (pickCalibrationValues . valuesInP1) input
    sum calibrationValues

valuesInP2 :: [Char] -> [Char]
valuesInP2 xs@('o':'n':'e' : _)= '1' : valuesInP2 (tail xs)
valuesInP2 xs@('t':'w':'o' : rest) = '2' : valuesInP2 (tail xs)
valuesInP2 xs@('t':'h':'r':'e':'e' : rest) = '3' : valuesInP2 (tail xs)
valuesInP2 xs@('f':'o':'u':'r' : rest) = '4' : valuesInP2 (tail xs)
valuesInP2 xs@('f':'i':'v':'e' : rest) = '5' : valuesInP2 (tail xs)
valuesInP2 xs@('s':'i':'x' : rest) = '6' : valuesInP2 (tail xs)
valuesInP2 xs@('s':'e':'v':'e':'n' : rest) = '7' : valuesInP2 (tail xs)
valuesInP2 xs@('e':'i':'g':'h':'t' : rest) = '8' : valuesInP2 (tail xs)
valuesInP2 xs@('n':'i':'n':'e': rest) = '9' : valuesInP2 (tail xs)
valuesInP2 (cur : rest) = if cur >= '1' && cur <= '9'   
    then cur : valuesInP2 rest 
    else valuesInP2 rest
valuesInP2 [] = []

partTwo :: [String] -> Int
partTwo input = do
    let calibrationValues = map (pickCalibrationValues . valuesInP2) input
    sum calibrationValues

main = do
    input <- readFile "input.txt"
    let inputLines = lines input
    let p1 = partOne inputLines
    let p2 = partTwo inputLines

    print "Part 1: \n"
    print p1
    print "Part 2: \n"
    print p2
