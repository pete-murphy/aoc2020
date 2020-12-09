module Day09.Main where

import AOCPrelude
import qualified Data.IntSet as IS

p1 :: String -> Maybe (IntSet, Int)
p1 = lines >>> map read >>> zipNext >>> find (not . isValid)

twentyFives :: [Int] -> [[Int]]
twentyFives = sequenceA (fmap (take 25) . drop <$> [0 ..])

uniqueSums :: [Int] -> IntSet
uniqueSums xs = IS.fromList do
  (x : ys) <- tails xs
  y <- ys
  pure (x + y)

zipNext :: [Int] -> [(IntSet, Int)]
zipNext xs = map uniqueSums (twentyFives xs) `zip` drop 25 xs

isValid :: (IntSet, Int) -> Bool
isValid (ns, n) = n `IS.member` ns

-- p2
contiguousSubsequences :: [a] -> [[a]]
contiguousSubsequences = concatMap (tail . inits) . tails

-- 🙈 pasting the answer I got from p1
n = 1930745883

p2 :: String -> Int
p2 =
  lines >>> map read >>> drop 25 >>> contiguousSubsequences >>> find (\xs -> sum xs == n)
    >>> \(Just xs) -> minimum xs + maximum xs

main = do
  input <- readFile "src/Day09/input"
  print (p1 input)
  print (p2 input)
