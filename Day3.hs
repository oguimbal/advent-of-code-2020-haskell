module Day3 where

items = do
  lns <- lines <$> readFile "./data/day3.txt"
  return $ map cycle lns

cnt r d = let
            mapInd f = zipWith f [0,r..]
            right = map head . mapInd drop
            down xs = [x|(x,i) <- zip xs [0..], i `mod` d == 0]
        in length . filter (=='#') . right . down

part1 = cnt 3 1 <$> items

--------------- PART 2 --------------------

cntAll = map (uncurry cnt) [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
cnt2 = do
    rep <- repeat <$> items
    return $ product $ zipWith ($) cntAll rep


-- >>> part2
-- 240

-- >>> part1
-- 2832009600
