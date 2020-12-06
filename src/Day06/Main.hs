module Day06.Main where

import AOCPrelude

p1 =
  splitOn "\n\n"
    >>> map (length . nub . foldr1 union . lines)
    >>> sum

p2 =
  splitOn "\n\n"
    >>> map (length . nub . foldr1 intersect . lines)
    >>> sum

main = do
  input <- readFile "src/Day06/input"
  print (p1 input)
  print (p2 input)