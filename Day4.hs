module Day4 where

import Data.Char
import Data.List.Split (splitWhen)
import qualified Data.Set as Set
import Numeric (readDec)

attrs txt =
  let vals = words txt
   in map (\(a : b : c : ':' : rs) -> (a : b : [c], rs)) vals

keys l = Set.fromList $ map fst l

parseList = map (attrs . unwords) . splitWhen (== "")

mandatory = Set.fromList ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

validsAttrs = filter (Set.isSubsetOf mandatory . keys) . parseList . lines <$> readFile "./data/day4.txt"

part1 = length <$> validsAttrs

-------------------------- PART 2 --------------------------------

fd c = all isDigit c && length c == 4

colors = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

valid x = case x of
  ("byr", c) -> fd c && read c >= 1920 && read c <= 2002
  ("iyr", c) -> fd c && read c >= 2010 && read c <= 2020
  ("eyr", c) -> fd c && read c >= 2020 && read c <= 2030
  ("hcl", '#' : col) -> all isHexDigit col && length col == 6
  ("hcl", _) -> False
  ("ecl", c) -> c `elem` colors
  ("pid", c) -> all isDigit c && length c == 9
  ("cid", _) -> True
  ("hgt", c) -> case readDec c of
    [(n, "cm")] -> n >= 150 && n <= 193
    [(n, "in")] -> n >= 59 && n <= 76
    _ -> False
  _ -> True

part2 = length . filter (all valid) <$> validsAttrs

-- >>>  part1
-- 170

-- >>> part2
-- 103
