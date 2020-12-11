module Day10.Main where

import AOCPrelude

p1 = lines >>> map read >>> sort >>> (0 :) >>> diffs >>> (\xs -> xs <> [3]) >>> countElem 1 &&& countElem 3 >>> uncurry (*)

p2 = lines >>> map read >>> sort >>> (0 :) >>> diffs >>> (\xs -> xs <> [3]) >>> countVariations

diffs :: [Int] -> [Int]
diffs xs = zipWith (-) (tail xs) xs

countVariations :: [Int] -> Int
countVariations = \case
  [] -> 1
  (3 : xs) -> countVariations xs
  xs ->
    span (== 1) xs
      & \(length -> x, xs') -> (tribs !! x) * countVariations xs'

tribs :: [Int]
tribs = 1 : 1 : 2 : map (sum . take 3) (tails tribs)

countElem :: Eq a => a -> [a] -> Int
countElem x = length . filter (== x)

main = do
  input <- readFile "src/Day10/input"
  print (p1 input)
  print (p2 input)