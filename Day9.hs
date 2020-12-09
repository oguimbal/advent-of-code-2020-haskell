module Day9 where

dat = map (read :: String -> Int) . lines <$> readFile "./data/day9.txt"

sums xs = [x + y | x <- xs, y <- xs, x /= y]

invalid _ _ [] = -1
invalid i acc (x : xs)
  | length acc < i = invalid i (x : acc) xs
  | x `elem` sums acc = invalid i (x : init acc) xs
  | otherwise = x

part1 = invalid 25 [] <$> dat

subseqs xs = [take l . drop i $ xs | i <- [0 .. length xs - 1], l <- [2 .. length xs - i]]

part2 = do
  rngs <- subseqs <$> dat
  inv <- part1
  return $ let w = head $ filter ((== inv) . sum) rngs
    in minimum w + maximum w

-- >>> part1
-- 10884537

-- >>> part2
-- 1261309
