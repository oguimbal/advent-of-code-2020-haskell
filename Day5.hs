module Day5 where

import Data.List
import Numeric (readInt)

-- binary-like reader
reader l o = readInt 2 (`elem` [l, o]) (fromEnum . (== l))

seat t =
  let [(row, s)] = reader 'B' 'F' t
      [(col, _)] = reader 'R' 'L' s
   in row * 8 + col

result = do
  seatIds <- map seat . lines <$> readFile "./data/day5.txt"
  return $
    let ids = sort . filter (>= 8) $ seatIds
        ziped = zip [53 ..] ids
     in ( maximum seatIds,
          fst <$> find (uncurry (/=)) ziped
        )

-- >>> result
-- (896,Just 659)
