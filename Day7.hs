module Day7 where

import Data.Char
import Data.Function
import Data.List.Split (splitOn)
import qualified Data.Set as Set

norm = filter isAlphaNum

ruleOf r =
  let [color, bagStr] = splitOn " bags contain " r
   in splitOn ", " bagStr
        & filter (/= "no other bags.")
        & map (unwords . init . words)
        & map norm
        & map (break isAlpha)
        & map (\(a, b) -> (norm color, (read a :: Int, b)))

allContainers rules color =
  let containers = map fst $ filter ((== norm color) . snd . snd) rules
   in color : concatMap (allContainers rules) containers

allContained rules color =
  let contained = map snd $ filter ((== norm color) . fst) rules
   in 1 + sum (map (\c -> fst c * (allContained rules . snd) c) contained)

ans = do
  rules <- concatMap ruleOf . lines <$> readFile "./data/day7.txt"
  return (
        Set.size . Set.fromList . filter (/= "shiny gold") $ allContainers rules "shiny gold",
        allContained rules "shiny gold" - 1
    )

-- >>> ans
-- (112,6260)
