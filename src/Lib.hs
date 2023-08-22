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
    pairSplit,
    parseCrates,
    execCrateInstruction,
    parseCrateInstruction,
    replaceNth,
    execCrateInstruction',
  )
where

import Data.Char (isAsciiLower, isNumber, ord)
import Data.List (transpose)

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

-- DAY 03

halfString :: String -> (String, String)
halfString xs = splitAt len xs
  where
    len = length xs `div` 2

letterPoint :: Char -> Integer
letterPoint x = toInteger $ if isAsciiLower x then ord x - 96 else ord x - 64 + 26

rucksackLetter :: (String, String) -> Char
rucksackLetter (a, b) = foldr (\x acc -> if x `elem` b then x else acc) '\0' a

rucksackGroupLetter :: String -> String -> String -> Char
rucksackGroupLetter a b c = rucksackLetter (foldr (\x acc -> if x `elem` c then x : acc else acc) [] b, a)

rucksackGroupScroll :: [String] -> Integer
rucksackGroupScroll [] = 0
rucksackGroupScroll (a : b : c : xs) = letterPoint (rucksackGroupLetter a b c) + rucksackGroupScroll xs

-- DAY 04

pairSplit :: String -> ((Integer, Integer), (Integer, Integer))
pairSplit xs =
  let pairs = map (`splitWithDelimiter` '-') $ splitWithDelimiter xs ','
   in strangeSplit $ map (map read) pairs
  where
    strangeSplit xs' = ((head $ head xs', head $ tail $ head xs'), (head $ head $ tail xs', head $ tail $ head $ tail xs')) -- This strange stuff does this [[Integer, Integer], [Integer, Integer]] -> ((Integer, Integer), (Integer, Integer))

-- DAY 05

parseCrates :: [String] -> [[Char]]
parseCrates xs = map head . takeWhile (not.null) $ iterate(drop 4) $ tail $ map (dropWhile(== ' ')) $ transpose xs

parseCrateInstruction :: String -> (Integer, Integer, Integer)
parseCrateInstruction xs = case words xs of
  [_, quantity, _, from, _, to] -> (read quantity, read from, read to)

execCrateInstruction :: (Integer, Integer, Integer) -> [[Char]] -> [[Char]]
execCrateInstruction (0, _, _) crates = crates
execCrateInstruction (qnt, from', to') crates =
  let from = from' - 1
      to = to' - 1
      takeColumn = crates !! fromIntegral from
      putColumn = crates !! fromIntegral to
      crateToMove = head takeColumn
      newTakeColumn = tail takeColumn
      newPutColumn = crateToMove : putColumn
   in execCrateInstruction (qnt - 1, from', to') $ replaceNth from newTakeColumn $ replaceNth to newPutColumn crates

execCrateInstruction' :: (Integer, Integer, Integer) -> [[Char]] -> [[Char]]
execCrateInstruction' (qnt, from', to') crates =
  let from = from' - 1
      to = to' - 1
      takeColumn = crates !! fromIntegral from
      putColumn = crates !! fromIntegral to
      crateToMove = take (fromIntegral qnt) takeColumn
      newTakeColumn = drop (fromIntegral qnt) takeColumn
      newPutColumn = crateToMove ++ putColumn
   in replaceNth from newTakeColumn $ replaceNth to newPutColumn crates

replaceNth :: Integer -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x : xs)
  | n == 0 = newVal : xs
  | otherwise = x : replaceNth (n - 1) newVal xs