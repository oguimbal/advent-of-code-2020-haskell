module Day2 where

import Data.Char

readInt str =
  let (n, rest) = span isDigit str
   in (read n, rest)

parse str =
  let (l, '-' : ls) = readInt str
      (h, ' ' : c : pwd) = readInt ls
   in (l, h, c, drop 2 pwd)

isValid (l, h, c, pwd) =
  let cnt = length $ filter (c ==) pwd
   in cnt >= l && cnt <= h

items = do
  lns <- lines <$> readFile "./data/day2.txt"
  return $ map parse lns

part1 = do length . filter isValid <$> items


--------------- PART 2 ---------------------
nth _ [] = Nothing
nth n (x : xs)
  | n <= 0 = Nothing
  | n == 1 = Just x
  | otherwise = nth (n -1) xs

isValid' (l, h, c, pwd) =
  let a = nth l pwd
      b = nth h pwd
  in (a == Just c) /= (b == Just c)

part2 = do length . filter isValid' <$> items

-- >>> part1
-- 655

-- >>> part2
-- 673
