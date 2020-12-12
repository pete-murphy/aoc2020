{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Day12.Main where

import AOCPrelude
import Control.Lens

instance MonadFail Identity where
  fail = error "whoops"

data Position = Position
  { _heading :: Int,
    _x :: Int,
    _y :: Int
  }
  deriving (Show, Eq)

makeLenses ''Position

type Parser = Parsec Void String

move :: String -> State Position Int
move (c : (read -> n)) = do
  pos' <- get
  let pos = case c of
        'N' -> pos' & y -~ n
        'S' -> pos' & y +~ n
        'E' -> pos' & x +~ n
        'W' -> pos' & x -~ n
        'R' -> pos' & heading +~ n
        'L' -> pos' & heading -~ n
        'F' -> case _heading pos' `mod` 360 of
          270 -> pos' & y -~ n -- North
          90 -> pos' & y +~ n -- South
          0 -> pos' & x +~ n -- East
          180 -> pos' & x -~ n -- West
          w -> error (show w)
  put pos
  pure (manhattanDistance pos)

p1 =
  lines
    >>> (\xs -> execState (traverse move xs) (Position 0 0 0))

manhattanDistance :: Position -> Int
manhattanDistance Position {..} = abs _x + abs _y

main = do
  -- input <- readFile "src/Day12/input-sample"
  input <- readFile "src/Day12/input"
  -- for_ (p1 input) print
  print (p1 input)