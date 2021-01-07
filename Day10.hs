module Day10 where

import Data.List

load :: ([Int] -> a) -> IO a
load fn = do
  dat <- map read . lines <$> readFile "./data/day10.txt"
  return $ fn dat

part1 lst =
  let srt = sort $ 0 : maximum lst + 3 : lst
      diffs = zipWith (-) (tail srt) srt
      ln i = length $ filter (== i) diffs
   in ln 3 * ln 1

-- >>> load part1
-- 1848

-- ============================ PART 2

part2 lst =
  let process :: [(Int, Int)] -> Int -> [(Int, Int)]
      process prec n =
        let accessible = takeWhile (\a -> (n - snd a) <= 3) prec
         in (sum $ map fst accessible, n) : accessible
   in fst $ head $ foldl process [(1, 0)] lst

-- >>> load (part2.sort)
-- 8099130339328
