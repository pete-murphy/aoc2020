module Day03.Main where

import AOCPrelude

data Square = Tree | Open deriving (Eq, Show)

type Pos = (Int, Int)

parseLine :: String -> [Square]
parseLine = cycle . map parseSquare

parseSquare :: Char -> Square
parseSquare '.' = Open
parseSquare '#' = Tree

(!!!) :: [[a]] -> (Int, Int) -> a
sqs !!! (x, y) = sqs !! x !! y

countSat :: (a -> Bool) -> [a] -> Int
countSat p = length . filter p

p1 = readFile "src/Day03/input" <&> sum . flip (zipWith (!!)) [0, 3 ..] . fmap (cycle . map (fromEnum . (== '#'))) . lines

p2 = do
  input <- readFile "src/Day03/input"
  let rows = parseLine <$> lines input
      f = countSat (== Tree) . map (rows !!!)
      t1 = f ([0 ..] `zip` [0 ..])
      t2 = f ([0 ..] `zip` [0, 3 ..])
      t3 = f ([0 ..] `zip` [0, 5 ..])
      t4 = f ([0 ..] `zip` [0, 7 ..])
      t5 = f ([0, 2 ..] `zip` [0, 1 ..])
  print (t1 * t2 * t3 * t4 * t5)
