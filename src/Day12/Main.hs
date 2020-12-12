{-# LANGUAGE TemplateHaskell #-}

module Day12.Main where

import AOCPrelude
import Control.Lens

data Position
  = Position
      { _heading :: Int,
        _x :: Int,
        _y :: Int
      }
  deriving (Show, Eq)

makeLenses ''Position

move :: String -> State Position Int
move (c : (read -> n)) = do
  pos' <- get
  let pos = case c of
        'N' -> pos' & y -~ n
        'S' -> pos' & y +~ n
        'E' -> pos' & x +~ n
        'W' -> pos' & x -~ n
        'L' -> pos' & heading -~ n
        'R' -> pos' & heading +~ n
        'F' -> case _heading pos' `mod` 360 of
          270 -> pos' & y -~ n -- North
          90 -> pos' & y +~ n -- South
          0 -> pos' & x +~ n -- East
          180 -> pos' & x -~ n -- West
      (a, b) = (_x pos, _y pos)
  put pos
  pure (manhattanDistance a b)

manhattanDistance :: Int -> Int -> Int
manhattanDistance n m = abs n + abs m

p1 =
  lines
    >>> (\xs -> evalState (traverse move xs) (Position 0 0 0))
    >>> last

-- p2

data Position'
  = Position'
      { _waypoint :: (Int, Int),
        _ship :: (Int, Int)
      }
  deriving (Show, Eq)

makeLenses ''Position'

move' :: String -> State Position' Int
move' (c : (read -> n)) = do
  pos' <- get
  let pos = case c of
          'N' -> pos' & waypoint . _2 -~ n
          'S' -> pos' & waypoint . _2 +~ n
          'E' -> pos' & waypoint . _1 +~ n
          'W' -> pos' & waypoint . _1 -~ n
          'L' -> pos' & waypoint %~ \(x, y) -> case n `mod` 360 of
            270 -> (negate y, x)
            90 -> (y, negate x)
            0 -> (x, y)
            180 -> (negate x, negate y)
          'R' -> pos' & waypoint %~ \(x, y) -> case n `mod` 360 of
            270 -> (y, negate x)
            90 -> (negate y, x)
            0 -> (x, y)
            180 -> (negate x, negate y)
          'F' ->
            let (x, y) = _waypoint pos'
             in pos' & ship %~ \(x', y') -> (x' + (n * x), y' + (n * y))
      (a, b) = _ship pos
  put pos
  pure (manhattanDistance a b)

p2 =
  lines
    >>> (\xs -> evalState (traverse move' xs) (Position' (10, (-1)) (0, 0)))
    >>> last

main = do
  input <- readFile "src/Day12/input"
  print (p1 input)
  print (p2 input)
