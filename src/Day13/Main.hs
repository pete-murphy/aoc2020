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
    >>> flip (!!) 1
    >>> splitOn ","
    >>> zip [0 :: Int ..]
    >>> (mapMaybe . traverse) (readMaybe @Int)

p2 :: String -> Int
p2 =
  parse2
    >>> sortOn snd
    >>> reverse
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
              let next = xs !! 1
               in f (map (dropWhile (< next)) xss)

main :: IO ()
main = do
  input <- readFile "src/Day13/input-sample"
  -- print (p1 input)
  print (p2 input)
