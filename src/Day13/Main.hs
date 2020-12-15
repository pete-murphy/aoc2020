{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}

module Day13.Main where

import AOCPrelude

parse1 = lines >>> map (splitOn ",") >>> (map . mapMaybe) (readMaybe @Int)

p1 =
  parse1
    >>> ( \([target] : ns : _) ->
            ns <&> \n ->
              head (dropWhile (< target) [0, n ..])
                & \n' -> (n, n', target)
        )
    >>> minimumBy (\(_, y, _) (_, y', _) -> compare y y')
    >>> (\(busId, t, t') -> (t - t') * busId)

-- p2

parse2 =
  lines
    >>> flip (!!) 1 -- grab the second line
    >>> splitOn ","
    >>> zip [0 :: Int ..] -- associate with index
    >>> (mapMaybe . traverse) (readMaybe @Int) -- drop 'x's

-- p2 :: String -> Int
p2 =
  parse2
    -- keep the largest interval at head of list for slight optimization 🤷‍♂️
    >>> sortOn snd
    >>> reverse
    -- make a list of infinite lists to search through
    >>> map
      ( \(x, y) ->
          let start = y - x
              next = start + y
           in [start, next ..]
      )
    >>> fix \f xss@(xs : _) ->
      let (x' : xs') = map head xss
       in if all (== x') xs'
            then x'
            else
              let !next = xs !! 1
               in f (map (dropWhile (< next)) xss)

main :: IO ()
main = do
  -- input <- readFile "src/Day13/input-sample"
  input <- readFile "src/Day13/input"
  -- print (p1 input)
  print (p2 "\n17,x,13,19") -- 3417
  print (p2 "\n67,7,59,61") -- 754018
  print (p2 "\n67,x,7,59,61") -- 779210
  print (p2 "\n67,7,x,59,61") -- 1261476
  print (p2 "\n1789,37,47,1889") -- 1202161486
  print (p2 input)

-- for_ (p2 input) print
