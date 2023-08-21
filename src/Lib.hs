module Lib
  ( splitWithDelimiter,
    splitByEmptyList,
    RPS (..),
    RPSRound,
    stringToRpsRound,
    rpsRoundPoints,
    stringToRpsRound2,
    halfString,
    letterPoint,
    rucksackLetter,
    rucksackGroupScroll,
  )
where

import Data.Char (isAsciiLower, ord)

-- DAY 01

splitWithDelimiter :: String -> Char -> [String]
splitWithDelimiter text delimiter = case break (== delimiter) text of
  (x, _ : xs) -> x : splitWithDelimiter xs delimiter
  (x, _) -> [x]

splitByEmptyList :: [String] -> [[String]]
splitByEmptyList =
  foldr
    ( \xs acc ->
        if null xs
          then [] : acc
          else (xs : head acc) : tail acc
    )
    [[]]

-- DAY 02

data RPS = Paper | Rock | Scissors
  deriving (Show, Eq)

type RPSRound = (RPS, RPS)

-- "A Z" -> (Rock, Scissors) [for day 2]
stringToRpsRound :: String -> RPSRound
stringToRpsRound [a, ' ', b] = (charToRps a, charToRps b)

charToRps :: Char -> RPS
charToRps x
  | x == 'A' || x == 'X' = Rock
  | x == 'B' || x == 'Y' = Paper
  | x == 'C' || x == 'Z' = Scissors

stringToRpsRound2 :: String -> RPSRound
stringToRpsRound2 [a, ' ', b] = (charToRps a, charToRps2 a b)

charToRps2 :: Char -> Char -> RPS
charToRps2 a b
  | b == 'Y' = charToRps a -- Draw
  | a == 'A' && b == 'X' || a == 'B' && b == 'Z' = Scissors
  | a == 'B' && b == 'X' || a == 'C' && b == 'Z' = Rock
  | a == 'C' && b == 'X' || a == 'A' && b == 'Z' = Paper

rpsToInt :: RPS -> Integer
rpsToInt Rock = 1
rpsToInt Paper = 2
rpsToInt Scissors = 3

rpsRoundPoints :: RPSRound -> Integer
rpsRoundPoints (a, b)
  | a == b = 3 + rpsToInt b -- Draw
  | a == Rock && b == Paper
      || a == Paper && b == Scissors
      || a == Scissors && b == Rock =
      6 + rpsToInt b -- Win
  | otherwise = 0 + rpsToInt b -- Loss

-- Day 03

halfString :: String -> (String, String)
halfString xs = splitAt len xs
  where
    len = length xs `div` 2

letterPoint :: Char -> Integer
letterPoint x = toInteger $ if isAsciiLower x then ord x - 96 else ord x - 64 + 26

rucksackLetter :: (String, String) -> Char
rucksackLetter (a, b) = foldr (\x acc -> if x `elem` b then x else acc) '\0' a

rucksackGroupLetter :: String -> String -> String -> Char
rucksackGroupLetter a b c = rucksackLetter(foldr (\x acc -> if x `elem` c then x:acc else acc) [] b, a)

rucksackGroupScroll :: [String] -> Integer
rucksackGroupScroll [] = 0
rucksackGroupScroll (a : b : c : xs) = letterPoint (rucksackGroupLetter a b c) + rucksackGroupScroll xs