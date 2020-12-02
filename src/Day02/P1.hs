module Day02.P1 where

import Data.Function
import Data.Void
import qualified Text.Megaparsec as P
import Text.Megaparsec.Char.Lexer

solve :: a -> b
solve = undefined

type Row = (Int, Int, Char, String)

type Parser = P.Parsec Void String

parseRow :: String -> Row
parseRow str =
  let (_min', '-' : rest) = span (/= '-') str
      (_max', rest') = span (/= ' ') rest
      (_char, ':' : ' ' : pass) = span (/= ':') (drop 1 rest')
      min' = read _min'
      max' = read _max'
      char = head _char
   in (min', max', char, pass)

sat :: Row -> Bool
sat (mn, mx, c, pass) = filter (== c) pass & length & \x -> x >= mn && x <= mx

sat' :: Row -> Bool
sat' (mn, mx, c, pass) =
  (c == (pass !! (mn - 1)) || c == (pass !! (mx - 1)))
    && not (c == (pass !! (mn - 1)) && c == (pass !! (mx - 1)))

main :: IO ()
main = do
  input <- readFile "src/Day02/input"
  input & lines
    & map parseRow
    & filter sat'
    & length
    & print