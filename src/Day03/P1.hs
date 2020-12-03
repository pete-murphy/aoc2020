module Day03.P1 where

data Square = Tree | Open deriving (Eq, Show)

type Pos = (Int, Int)

parseLine :: String -> [Square]
parseLine = cycle . map parseSquare

parseSquare :: Char -> Square
parseSquare '.' = Open
parseSquare '#' = Tree

-- Just horizontal position
trail1 :: [Int]
trail1 = [0 ..]

trail2 :: [Int]
trail2 = [0, 3 ..]

trail3 :: [Int]
trail3 = [0, 5 ..]

trail4 :: [Int]
trail4 = [0, 7 ..]

trail5_ :: [Int]
trail5_ = [0, 1 ..]

squares5 xs = map snd $ filter (\(i, _) -> even i) ([0 ..] `zip` xs)

trailToTrees :: [Int] -> [[Square]] -> Int
trailToTrees trail lns =
  length
    (filter (== Tree) (zipWith (!!) lns trail))

main :: IO ()
main = do
  input <- readFile "src/Day03/input"
  let rows = parseLine <$> lines input
      t1 = trailToTrees trail1 rows
      t2 = trailToTrees trail2 rows
      t3 = trailToTrees trail3 rows
      t4 = trailToTrees trail4 rows
      t5 = trailToTrees trail5_ (squares5 rows)
  print (t1 * t2 * t3 * t4 * t5)
