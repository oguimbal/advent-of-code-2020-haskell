module Day11 where

import Utils
import Data.List

type State = (Pos, Pos)

type Inst = (Char, Int)

load fn = fn . parse <$> readFile "./data/day12.txt"

parse s = map pline $ lines s
  where
    pline :: String -> Inst
    pline (x : xs) = (x, read xs :: Int)

man (a, b) = abs a + abs b

dir 'E' = (1, 0)
dir 'W' = (-1, 0)
dir 'N' = (0, 1)
dir 'S' = (0, -1)

rot :: Int -> Pos -> Pos
rot n = r `fnPow` (n `mod` 4)
  where
    r (x, y) = (- y, x)

step :: State -> Inst -> State
step (p, v) (d, n) = case d of
  'L' -> (p, rot (n `div` 90) v) -- rotate
  'R' -> step (p, v) ('L', - n)
  'F' -> (p + n *- v, v) -- move in the direction of the velocity
  _ -> (p + n *- dir d, v) -- move in the given direction

part1 = load $ man . fst . foldl' step ((0, 0), dir 'E')

-- >>> part1
-- 2879

-------------------- PART 2

step' :: State -> Inst -> State
step' (p, v) (d, n) = case d of
  'L' -> (p, rot (n `div` 90) v) -- rotate
  'R' -> step (p, v) ('L', - n)
  'F' -> (p + n *- v, v) -- move in the direction of the velocity
  _ -> (p, v + n *- dir d) -- move in the given direction

part2 = load $ man . fst . foldl' step' ((0, 0), (10, 1))

-- >>> part2
-- 178986
