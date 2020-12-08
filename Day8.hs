module Day8 where


parse (a : b : c : _ : s : xs) =
  (a : b : [c], if s == '-' then negate x else x)
  where
    x = read xs :: Int
data Type = Bug Int | Success Int deriving (Eq, Show)
result = do
  ops <- map parse . lines <$> readFile "./data/day8.txt"
  return $
    let run ri =
          let get i
                | i == ri = case ops !! i of
                  ("jmp", n) -> ("nop", n)
                  ("nop", n) -> ("jmp", n)
                  x -> x
                | otherwise = ops !! i
              fails h i a
                | i `elem` h || i > length ops || i < 0 = Bug a
                | i == length ops = Success a
                | otherwise = case get i of
                  ("jmp", n) -> fails (i : h) (i + n) a
                  ("acc", n) -> fails (i : h) (i + 1) (a + n)
                  _ -> fails (i : h) (i + 1) a
           in fails [] 0 0
     in  (-- part1
          run (-1),
          -- part2
         [x | x@Success {} <-  map run [1 .. length ops]]
     )


-- >>> result
-- (Bug 1446,[Success 1403])
