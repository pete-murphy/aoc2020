module Day02.Main where

import AOCPrelude
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P

solve :: a -> b
solve = undefined

type Row = (Int, Int, Char, String)

type Parser = P.Parsec Void String

parseRow' :: Parser Int
parseRow' = do
  mn <- P.decimal <* P.char '-'
  mx <- P.decimal <* P.spaceChar
  c <- P.anySingle <* P.skipMany (P.char ':' <|> P.spaceChar)
  pass <- P.manyTill P.asciiChar P.newline
  let numCs = c `countElem` pass
  pure do
    fromEnum (numCs >= mn && numCs <= mx)

countElem :: Eq a => a -> [a] -> Int
countElem i = length . filter (== i)

parseRow'' :: Parser Int
parseRow'' = do
  mn <- P.decimal <* P.char '-'
  mx <- P.decimal <* P.spaceChar
  c <- P.anySingle <* P.skipMany (P.char ':' <|> P.spaceChar)
  pass <- P.manyTill P.asciiChar P.newline
  pure do
    fromEnum ((c == pass !! (mn - 1)) /= (c == pass !! (mx - 1)))

main :: IO ()
main = do
  readFile "src/Day02/input"
    >>= (P.runParser (P.many parseRow'') "" >>> fmap sum >>> print)
