module Day11.Main where

import AOCPrelude
import qualified Data.Map as M

parse1 :: String -> Map (Int, Int) Char
parse1 =
  lines >>> map (zip [0 ..]) >>> zip [0 ..]
    >>> map (\(i, xs) -> map (\(j, c) -> ((i, j), c)) xs)
    >>> concat
    >>> M.fromAscList

-- >>> \xs -> array ((0, 0), (length xs - 1, length (head xs) - 1)) xs

p1 =
  parse1 >>> nTimes 219 tick
    >>> M.filter (== '#')
    >>> length

nTimes :: Int -> (a -> a) -> a -> a
nTimes 0 _ a = a
nTimes n f a = f (nTimes (n -1) f a)

repeatUntilDone :: Eq a => (a -> a) -> a -> a
repeatUntilDone f xs = if f xs == xs then xs else repeatUntilDone f xs

tick :: Map (Int, Int) Char -> Map (Int, Int) Char
tick mp =
  M.mapWithKey (\i seat -> tick' seat (getNeighbors mp i)) mp

-- listArray (bounds xs) (fmap (\ix -> tick' (xs ! ix) (getNeighbors xs ix)) (indices xs))

tick' :: Char -> [Char] -> Char
tick' 'L' ns
  | '#' `notElem` ns = '#'
  | otherwise = 'L'
tick' '#' ns
  | '#' `countElem` ns >= 4 = 'L'
  | otherwise = '#'
tick' s _ = s

getNeighbors :: Map (Int, Int) Char -> (Int, Int) -> [Char]
getNeighbors xs (i, j) =
  catMaybes do
    (xs M.!?)
      <$> [ (i - 1, j - 1),
            (i, j - 1),
            (i - 1, j),
            (i, j + 1),
            (i + 1, j),
            (i + 1, j + 1),
            (i - 1, j + 1),
            (i + 1, j - 1)
          ]

main = do
  input <- readFile "src/Day11/input"
  print (p1 input)

countElem :: Eq a => a -> [a] -> Int
countElem i = length . filter (== i)
