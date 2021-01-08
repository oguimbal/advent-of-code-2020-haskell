module Utils where

type Pos = (Int, Int)

-- Vector algebra
instance (Num a, Num b) => Num (a, b) where
  (x, y) + (a, b) = (x + a, y + b)
  (x, y) * (a, b) = (x * a, y * b)
  abs (x, y) = (abs x, abs y)
  signum (x, y) = (signum x, signum y)
  fromInteger x = (fromInteger x, 0)
  negate (x, y) = (negate x, negate y)

(*-) :: Int -> Pos -> Pos
(*-) n (x, y) = (n * x, n * y)

-- Apply a function N times
fnPow :: (a -> a) -> Int -> a -> a
fnPow _ 0 v = v
fnPow fn 1 v = fn v
fnPow fn n v = fn $ fnPow fn (n -1) v
