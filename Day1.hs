module Day1 where

result = do
  nums <- map (read :: String -> Int) . lines <$> readFile "./data/day1.txt"
  return
    ( head [x * y | x <- nums, y <- nums, x + y == 2020],
      head [x * y * z | x <- nums, y <- nums, z <- nums, x + y + z == 2020]
    )

-- >>> result
-- (485739,161109702)
