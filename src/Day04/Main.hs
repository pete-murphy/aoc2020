module Day04.Main where

import AOCPrelude

main :: IO ()
main = do
  input <- readFile "input"
  input
    & splitOn "\n\n"
    & map words
    & (map . map) (second (drop 1) . span (/= ':'))
    & filter (isJust . sequence . isValid)
    & length
    & print

isValid :: [(String, String)] -> [Maybe String]
isValid xs =
  [ lookup "byr" xs >>= (readMaybe >=> fromPred (isBetween 1920 2002)) & fmap show,
    lookup "iyr" xs >>= (readMaybe >=> fromPred (isBetween 2010 2020)) & fmap show,
    lookup "eyr" xs >>= (readMaybe >=> fromPred (isBetween 2020 2030)) & fmap show,
    lookup "hgt" xs >>= fromPred parseHgt,
    lookup "hcl" xs >>= fromPred parseHcl,
    lookup "ecl" xs >>= fromPred parseEcl,
    lookup "pid" xs >>= parsePid
  ]

parsePid :: String -> Maybe String
parsePid = fromPred (\xs -> length xs == 9 && all isDigit xs)

parseEcl :: String -> Bool
parseEcl s = s `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

parseHcl :: String -> Bool
parseHcl ('#' : rest) = all isHexDigit rest
parseHcl _ = False

parseHgt :: String -> Bool
parseHgt xs =
  case span isDigit xs of
    (read -> n, "cm") -> isBetween 150 193 n
    (read -> n, "in") -> isBetween 59 76 n
    _ -> False

isBetween mn mx x = mn <= x && mx >= x

fromPred p x = if p x then Just x else Nothing