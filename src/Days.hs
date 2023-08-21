module Days where

import Data.List (sort, sortBy)
import Data.Ord (Down (Down), comparing)
import Lib

one :: IO ()
one = do
  putStrLn "\n## DAY 01 ##"
  input <- readFile "inputs/01.txt"
  -- Split with \n and \n\n
  let splitted = splitByEmptyList $ splitWithDelimiter input '\n'

  -- Cast every String to Integer
  let splittedInt = map (map read) splitted

  -- Sum every [Integer] inside [[Integer]]
  let sums = sortBy (comparing Down) (map sum splittedInt)

  print $ head sums
  putStrLn "1st part DONE"
  -- Print the sum of the top 3 elves
  print $ sum $ take 3 sums
  putStrLn "2nd part DONE"

two :: IO ()
two = do
  putStrLn "\n## DAY 02 ##"
  input <- readFile "inputs/02.txt"
  -- Split with \n
  let splitted = splitWithDelimiter input '\n'

  let rounds = map stringToRpsRound splitted
  let totalPoints = foldr ((+) . rpsRoundPoints) 0 rounds
  print totalPoints
  putStrLn "1st part DONE"

  let rounds' = map stringToRpsRound2 splitted
  let totalPoints' = foldr ((+) . rpsRoundPoints) 0 rounds'
  print totalPoints'
  -- print rounds'
  putStrLn "2nd part DONE"

three :: IO ()
three = do
  putStrLn "\n## DAY 03 ##"
  input <- readFile "inputs/03.txt"
  let splitted = splitWithDelimiter input '\n'
  let rucksack = map halfString splitted
  let letter = map rucksackLetter rucksack
  let points = map letterPoint letter
  -- print $ zip letter points
  print $ sum points
  putStrLn "1st part DONE"

  let points' = rucksackGroupScroll splitted
  print points'
  putStrLn "2nd part DONE"

