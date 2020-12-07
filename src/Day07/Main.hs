{-# LANGUAGE TupleSections #-}

module Day07.Main where

import AOCPrelude
import qualified Data.Map as M
import qualified Data.Set as S

data Bag = Bag String String
  deriving (Eq, Ord, Show)

type BagPolicy = Map Bag (Set (Int, Bag))

parseRow :: String -> (Bag, Set (Int, Bag))
parseRow (words -> (b1 : b2 : "bags" : "contain" : rest)) =
  (Bag b1 b2, S.fromList (go [] rest))
  where
    go acc ("no" : _) = acc
    go acc (n : x1 : x2 : _ : rest) = go ((read n, Bag x1 x2) : acc) rest
    go acc _ = acc

accumBagPolicy :: [(Bag, Set (Int, Bag))] -> Map Bag (Set (Int, Bag))
accumBagPolicy = M.fromListWith S.union

accum'BagPolicy :: BagPolicy -> BagPolicy
accum'BagPolicy m =
  M.foldrWithKey (M.insertWith S.union) m m

p1 =
  lines
    >>> map parseRow
    >>> accumBagPolicy
    >>> (\m -> filter (hasShinyGold m) (M.toList m))
    >>> map fst
    >>> length

p2 =
  lines
    >>> map parseRow
    >>> accumBagPolicy
    >>> shinyGoldHas

shinyGold = Bag "shiny" "gold"

hasShinyGold :: Map Bag (Set (Int, Bag)) -> (Bag, Set (Int, Bag)) -> Bool
hasShinyGold m (b, s) =
  -- Check if shiny gold can be contained directly
  S.member shinyGold (S.map snd s)
    -- Or indirectly by some child bag
    || or do
      let check = hasShinyGold m
          childBags = S.map snd s
          xs = catMaybes (S.toList childBags <&> \k -> M.lookup k m)
       in check <$> map (b,) xs

shinyGoldHas :: Map Bag (Set (Int, Bag)) -> Int
shinyGoldHas m =
  M.lookup shinyGold m
    & \(Just s) -> foldr (\(n, b) acc -> acc + (n + n * go b)) 0 s
  where
    go b =
      M.lookup b m
        & \(Just s) -> foldr (\(n, b) acc -> acc + (n + n * go b)) 0 s

main = do
  input <- readFile "src/Day07/input"

  print (p1 input)
  print (p2 input)