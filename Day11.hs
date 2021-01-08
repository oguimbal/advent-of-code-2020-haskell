module Day11 where

import Utils
import Data.List (intercalate)
import Data.Maybe
import qualified Data.Vector as V

load fn = fn <$> readFile "./data/day11.txt"

data Cell = Floor | Occupied | Empty deriving (Eq)

instance Show Cell where
  show x = [toChar x]

toChar :: Cell -> Char
toChar Floor = '.'
toChar Occupied = '#'
toChar Empty = 'L'



type Dim = Pos

data Map = Map
  { cells :: V.Vector Cell,
    dim :: Dim
  }
  deriving (Eq)

w = fst . dim

h = snd . dim

line :: Map -> Int -> V.Vector Cell
line m i = V.take (w m) . V.drop (i * w m) $ cells m

instance Show Map where
  show m =
    let lns = map (line m) [0 .. h m]
        dat = map (V.toList . V.map toChar) lns
     in show (dim m) ++ " -> " ++ intercalate " | " dat

------------------------- Parsing
readCell c = case c of
  'L' -> Empty
  '#' -> Occupied
  _ -> Floor

parse :: String -> Map
parse str =
  Map
    { cells = V.fromList $ map readCell $ filter (/= '\n') str,
      dim = (length $ head $ lines str, length $ lines str)
    }

------------------------- Utilities

-- Get a cell value
get :: Pos -> Map -> Cell
get (x, y) v = cells v V.! (x + y * w v)

getSafe :: Pos -> Map -> Maybe Cell
getSafe p@(x, y) m@Map {dim = (w, h)} = if x >= 0 && y >= 0 && x < w && y < h then Just (get p m) else Nothing

type Lookup = Pos -> Map -> [Cell]

directions :: [Pos]
directions =
  [ (i, j)
    | j <- [-1 .. 1],
      i <- [-1 .. 1],
      i /= 0 || j /= 0
  ]

-- Get adjascent cells
adjascents :: Lookup
adjascents p m = mapMaybe ((`getSafe` m) . (+) p) directions

toList :: Map -> [(Pos, Cell)]
toList m = zip pos (V.toList $ cells m)
  where
    pos =
      [ (x, y)
        | y <- [0 .. h m - 1],
          x <- [0 .. w m - 1]
      ]

fromList :: Pos -> [Cell] -> Map
fromList p c =
  Map
    { cells = V.fromList c,
      dim = p
    }

-- step :: Map -> Map
step :: (Pos -> Map -> [Cell]) -> Int -> Map -> Map
step fn n m =
  let trans :: (Pos, Cell) -> Cell
      trans (p, c)
        -- If a seat is empty (L) and there are no occupied seats adjacent to it, the seat becomes occupied.
        | c == Empty && occ == 0 = Occupied
        -- If a seat is occupied (#) and four or more seats adjacent to it are also occupied, the seat becomes empty.
        | c == Occupied && occ >= n = Empty
        -- Otherwise, the seat's state does not change.
        | otherwise = c
        where
          occ = length $ filter (== Occupied) $ fn p m
   in fromList (dim m) $ map trans $ toList m

stabilize :: Lookup -> Int -> Map -> Map
stabilize fn n m =
  let m' = step fn n m
   in if m' == m
        then m
        else stabilize fn n m'

run :: Lookup -> Int -> String -> Int
run lookup n =
  length
    . filter (== Occupied)
    . map snd
    . toList
    . stabilize lookup n
    . parse

part1 =
  load $ run adjascents 4

-- >>> part1
-- 2296

------------------------- PART 2



firstSeat :: Pos -> Map -> Pos -> Maybe Cell
firstSeat p m d =
  let p' = p + d
      ret = getSafe p' m
   in case ret of
        Just Floor -> firstSeat p' m d
        _ -> ret

firstSeatPos :: Pos -> Map -> Pos -> Maybe Pos
firstSeatPos d m p =
  let p' = p + d
      ret = getSafe p' m
   in case ret of
        Just Floor -> firstSeatPos d m p'
        Nothing -> Nothing
        _ -> Just p'

visibles :: Lookup
visibles p m = mapMaybe (firstSeat p m) directions

part2 =
  load $ run visibles 5

-- >>> part2
-- 2089
