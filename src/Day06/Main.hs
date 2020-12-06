module Day06.Main where

import AOCPrelude
import qualified Data.Set as S

parseGroups :: String -> [String]
parseGroups = splitOn "\n\n"

parsePersons :: [String] -> String
parsePersons = foldr1 union

uniqPerson :: [Char] -> Int
uniqPerson = S.size . S.fromList

solve = do
  input <- readFile "src/Day06/input"
  input
    & parseGroups
    & map lines
    & map parsePersons
    & map uniqPerson
    & sum
    & print

-- & lines
-- & map parseLine
-- & map (\(r, c) -> (head $ parseRow r, head $ parseCol c))
-- & map (\(r, c) -> r * 8 + c)
-- & maximum
-- & print

-- solve' = do
--   input <- readFile "src/Day06/input"
--   input
--     & lines
--     & map parseLine
--     & map (\(r, c) -> (head $ parseRow r, head $ parseCol c))
--     & map (\(r, c) -> r * 8 + c)
--     & sort
--     & (\xs -> [head xs ..] `zip` xs)
--     & find (uncurry (/=))
--     & print
