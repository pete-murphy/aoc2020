module Day08.Main where

import AOCPrelude
import Data.List.Index
import Data.Sequence (Seq (..), (!?))
import qualified Data.Sequence as Seq
import qualified Data.Set as S

p1 :: String -> Int
p1 = lines >>> map parseLine >>> Seq.fromList >>> interpret

p2 :: String -> Int
p2 = lines >>> map parseLine >>> variations >>> map Seq.fromList >>> head . mapMaybe interpret'

interpret :: Seq.Seq Instr -> Int
interpret sq = go 0 S.empty 0 (sq `Seq.index` 0)
  where
    go i s acc (Nop _) = go (i + 1) (S.insert i s) acc (sq `Seq.index` (i + 1))
    go i s acc (Acc n) = go (i + 1) (S.insert i s) (acc + n) (sq `Seq.index` (i + 1))
    go i s acc (Jmp n) =
      if (i + n) `S.member` s
        then acc
        else go (i + n) (S.insert i s) acc (sq `Seq.index` (i + n))

interpret' :: Seq.Seq Instr -> Maybe Int
interpret' sq = go 0 S.empty 0 (sq `Seq.index` 0)
  where
    go
      i -- Current index, used to lookup the current instruction, and add to "seen indices" on each tick
      s -- Set of "seen indices"
      acc -- Accumulator
      instr -- Current instruction, got by indexing the input 'sq' on each tick
      -- If we reach the end of input, we succeed and return the accumulator
        | i == Seq.length sq = Just acc
        | otherwise = case instr of
          Nop _ -> go (i + 1) (S.insert i s) acc (sq `Seq.index` (i + 1))
          Acc n -> go (i + 1) (S.insert i s) (acc + n) (sq `Seq.index` (i + 1))
          Jmp n ->
            if (i + n) `S.member` s
              then -- We'd be looping at this point, so this is a failure case
                Nothing
              else go (i + n) (S.insert i s) acc (sq `Seq.index` (i + n))

variations :: [Instr] -> [[Instr]]
variations instrs =
  let nops = map (\i -> updateAt i swapNop instrs) [0 .. length instrs]
      jmps = map (\i -> updateAt i swapJmp instrs) [0 .. length instrs]
      swapNop (Nop n) = Just (Jmp n)
      swapNop x = Just x
      swapJmp (Jmp n) = Just (Nop n)
      swapJmp x = Just x
   in nops <> jmps

data Instr = Acc Int | Nop Int | Jmp Int
  deriving (Show)

parseLine :: String -> Instr
parseLine (words -> (instr : (splitAt 1 -> (s, read -> n)) : _)) =
  case instr of
    "acc" -> if s == "+" then Acc n else Acc (negate n)
    "jmp" -> if s == "+" then Jmp n else Jmp (negate n)
    "nop" -> if s == "+" then Nop n else Nop (negate n)
    _ -> error "Unhandled instruction"

main = do
  input <- readFile "src/Day08/input"

  print (p1 input)
  print (p2 input)