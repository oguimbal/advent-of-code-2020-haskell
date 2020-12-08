module Day6 where

import Data.List
import Data.List.Split (splitWhen)
import qualified Data.Set as Set

groups = splitWhen (== "") . lines <$> readFile "./data/day6.txt"
part1 = sum . map (length . Set.fromList . intercalate "") <$> groups
part2 = sum . map (Set.size . foldl1 Set.intersection . map Set.fromList)  <$> groups


-- >>> part1
-- 6878

-- >>> part2
-- 3464
