module Main where

import System.IO (readFile)
import Data.List
import qualified Data.Map as Map


main :: IO ()
main = do
  contents <- readFile "../../src/main/resources/day01_input1.txt"
  let linesOfFile = lines contents
      pairs = [ (read l, read r) | line <- linesOfFile, let [l, r] = words line ]
      (left, right) = unzip pairs
    
  print $ part1 left right
  print $ part2 left right

part1 :: [Int] -> [Int] -> Int
part1 left right = sum $ zipWith (\l r -> abs (l - r)) (sort left) (sort right)

part2 :: [Int] -> [Int] -> Int
part2 left right = sum [ v * Map.findWithDefault 0 v rightFreq | v <- left ]
  where
    rightFreq = Map.fromListWith (+) [(v, 1) | v <- right]
