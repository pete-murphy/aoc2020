module Day11.Main where

import AOCPrelude
import qualified Data.Map as M

parse1 :: String -> Map (Int, Int) Char
parse1 =
  lines >>> map (zip [0 ..]) >>> zip [0 ..]
    >>> map (\(i, xs) -> map (\(j, c) -> ((i, j), c)) xs)
    >>> concat
    >>> M.fromAscList

p1 =
  parse1 >>> repeatUntilDone tick
    >>> M.filter (== '#')
    >>> length

repeatUntilDone :: Eq a => (a -> a) -> a -> a
repeatUntilDone f xs = go (f xs) xs
  where
    go curr last = if curr == last then curr else go (f curr) curr

tick :: Map (Int, Int) Char -> Map (Int, Int) Char
tick mp =
  M.mapWithKey (\i seat -> tick' seat (getNeighbors mp i)) mp

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

--- p2

getNeighbors2 :: Map (Int, Int) Char -> (Int, Int) -> [Char]
getNeighbors2 xs (i, j) =
  catMaybes $
    (\xs -> if null xs then Nothing else head xs)
      . dropWhile (== Just '.')
      . takeWhile isJust
      . map (xs M.!?)
      <$> [ [i - 1, i - 2 ..] `zip` [j - 1, j - 2 ..],
            repeat i `zip` [j - 1, j - 2 ..],
            [i - 1, i - 2 ..] `zip` repeat j,
            repeat i `zip` [j + 1, j + 2 ..],
            [i + 1, i + 2 ..] `zip` repeat j,
            [i + 1, i + 2 ..] `zip` [j + 1, j + 2 ..],
            [i - 1, i - 2 ..] `zip` [j + 1, j + 2 ..],
            [i + 1, i + 2 ..] `zip` [j - 1, j - 2 ..]
          ]

draw :: Map (Int, Int) Char -> [String]
draw = M.toAscList >>> splitWhen (\((_, x), _) -> x == 0) >>> map (map snd)

tick2 :: Map (Int, Int) Char -> Map (Int, Int) Char
tick2 mp =
  unsafePerformIO (hSetBuffering stdout NoBuffering *> hPrint stdout (draw mp))
    `seq` M.mapWithKey (\i seat -> tick2' seat (getNeighbors2 mp i)) mp

tick2' :: Char -> [Char] -> Char
tick2' 'L' ns
  | '#' `notElem` ns = '#'
  | otherwise = 'L'
tick2' '#' ns
  | '#' `countElem` ns >= 5 = 'L'
  | otherwise = '#'
tick2' s _ = s

p2 =
  parse1 >>> repeatUntilDone tick2
    >>> M.filter (== '#')
    >>> length

main = do
  input <- readFile "src/Day11/input"
  -- print (p1 input)
  print (p2 input)

countElem :: Eq a => a -> [a] -> Int
countElem i = length . filter (== i)
