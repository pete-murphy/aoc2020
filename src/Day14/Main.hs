{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Day14.Main where

import AOCPrelude
import Control.Lens
import qualified Data.Map as M
import qualified Data.Text.IO as T
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P

type Parser = Parsec Void Text

type Address = Int

type Mask = [Maybe Bool]

type Memory = Map Address [Bool]

data Program
  = Program
      { _mask :: Mask,
        _instructions :: [(Address, [Bool])]
      }
  deriving (Show)

makeLenses ''Program

toBinary :: Int -> [Bool]
toBinary = padTill 36 False
  . reverse
  . unfoldr \rem ->
    if rem == 0
      then Nothing
      else
        let (rem', x) = rem `divMod` 2
         in Just (toEnum x, rem')

fromBinary :: [Bool] -> Int
fromBinary = sum . zipWith (\x y -> y * (2 ^ x)) [0 ..] . reverse . map fromEnum

displayBinary :: [Bool] -> String
displayBinary = map (head . show . fromEnum)

padTill :: Int -> a -> [a] -> [a]
padTill n x xs =
  replicate (n - length xs) x <> xs

programP :: Parser Program
programP = do
  _mask <-
    string "mask = "
      *> count 36 do
        choice
          [ char 'X' $> Nothing,
            char '0' $> Just False,
            char '1' $> Just True
          ]
  newline
  _instructions <-
    many do
      string "mem"
      addr <- between (char '[') (char ']') decimal
      string " = "
      n <- decimal
      void newline <|> eof
      pure (addr, toBinary n)
  pure Program {..}

applyMask :: Program -> [(Address, [Bool])]
applyMask Program {..} =
  _instructions <&> fmap (applyMask' _mask)
  where
    applyMask' :: Mask -> [Bool] -> [Bool]
    applyMask' ms bs = catMaybes (zipWith (<|>) ms (map Just bs))

p1 :: Text -> Int
p1 =
  parse (many programP)
    >>> either (\_ -> error "Failed to parse") id
    >>> concatMap applyMask
    >>> M.fromList
    >>> sumOf (folded . to fromBinary)

main :: IO ()
main = do
  input <- T.readFile "src/Day14/input"
  -- input <- T.readFile "src/Day14/input"
  -- mapM_ (traverse_ (putStrLn . unlines . map displayBinary . map snd . _instructions)) (parse (many programP) input)
  print (p1 input)
