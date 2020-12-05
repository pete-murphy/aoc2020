module Day03.Main where

import AOCPrelude
import Data.Functor ((<&>))

data Square = Tree | Open deriving (Eq, Show)

type Pos = (Int, Int)

parseLine :: String -> [Square]
parseLine = cycle . map parseSquare

parseSquare :: Char -> Square
parseSquare '.' = Open
parseSquare '#' = Tree

-- -- Just horizontal position
-- trail1 :: [Int]
-- trail1 = [0 ..]

-- trail2 :: [Int]
-- trail2 = [0, 3 ..]

-- trail3 :: [Int]
-- trail3 = [0, 5 ..]

-- trail4 :: [Int]
-- trail4 = [0, 7 ..]

-- trail5_ :: [Int]
-- trail5_ = [0, 1 ..]

-- squares5 xs = map snd $ filter (\(i, _) -> even i) ([0 ..] `zip` xs)

-- (!!!) :: [[a]] -> (Int > Int -> a
-- sqs !!! (x, y) = sqs !! x !! y

-- countSat :: (a -> Bool) -> [a] -> Int
-- countSat p = length . filter p

-- main :: IO ()
-- main = do
--   input <- readFile "src/Day03/input"
--   let rows = parseLine <$> lines input
--       t1 = map (rows !!!) ([0..] `zip` [0..])
--       t2 = map (rows !!!) ([0..] `zip` [0,3..])
--       t3 =  map (rows !!!) [(0,0), (1,5)..]
--       t4 =  map (rows !!!) [(0,0), (1,7)..]
--       t5 =  map (rows !!!) [(0,0), (2,1)..]
--   print (t1 * t2 * t3 * t4 * t5)

main = readFile "src/Day03/input" <&> sum . flip (zipWith (!!)) [0, 3 ..] . fmap (cycle . map (fromEnum . (== '#'))) . lines