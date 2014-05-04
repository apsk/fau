-- in progress...
module CF_P2B where -- http://codeforces.ru/problemset/problem/2/B

import Data.Array
import Array
import Misc
import DP

type Path = String

cost Nothing             = 0
cost (Just (d5, d2, d0)) = min d5 d2 * d0

solve :: Array Int2 Integer -> (Int, Path)
solve mx = mapFst cost (dpMapMxRD mapper mx ! (rows, cols))
  where
    (rows, cols) = snd $ bounds mx
    mapper lookup (r, c) e = case (lookup (r - 1, c), lookup (r, c - 1)) of
      (Nothing, Nothing) -> (Just (0, 0, 0), "")
      (Just sp, Nothing) -> sp
      (Nothing, Just sp) -> sp
      (Just (s1, p1), Just (s2, p2)) ->
        minBy (cost . fst) (updateCost s1, 'D' : p1) (updateCost s2, 'R' : p2)
      where
        updateCost Nothing = Nothing
        updateCost (Just c@(d10, d5, d2))
          | e `mod` 10 == 0 = Just (d10 + trailingZeros e, d5, d2)
          | e `mod` 5  == 0 = Just (d10, d5 + 1, d2)
          | e `mod` 2  == 0 = Just (d10, d5, d2 + 1)
          | otherwise       = Just c

main = do
  dim <- readLn
  mx <- mxRead dim dim
  let (c, p) = solve mx
  print c
  putStr p
