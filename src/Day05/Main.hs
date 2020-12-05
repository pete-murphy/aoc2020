module Day05.Main where

import AOCPrelude

parseLine :: String -> (String, String)
parseLine = splitAt 7

parseRow :: String -> [Int]
parseRow = go [0 .. 127]
  where
    go ns "" = ns
    go ns ('F' : rest) = go (take (length ns `div` 2) ns) rest
    go ns ('B' : rest) = go (drop (length ns `div` 2) ns) rest

parseCol :: String -> [Int]
parseCol = go [0 .. 7]
  where
    go ns "" = ns
    go ns ('R' : rest) = go (drop (length ns `div` 2) ns) rest
    go ns ('L' : rest) = go (take (length ns `div` 2) ns) rest

solve = do
  input <- readFile "src/Day05/input"
  input
    & lines
    & map parseLine
    & map (\(r, c) -> (head $ parseRow r, head $ parseCol c))
    & map (\(r, c) -> r * 8 + c)
    & maximum
    & print

solve' = do
  input <- readFile "src/Day05/input"
  input
    & lines
    & map parseLine
    & map (\(r, c) -> (head $ parseRow r, head $ parseCol c))
    & map (\(r, c) -> r * 8 + c)
    & sort
    & (\xs -> [head xs ..] `zip` xs)
    & find (uncurry (/=))
    & print
