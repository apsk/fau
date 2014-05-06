module CF_P2B where -- http://codeforces.ru/problemset/problem/2/B

import Data.Array
import Control.Arrow

import Array
import Misc
import DP

type Path = String
type Matrix = Array Int2 Integer

cost Nothing             = 0
cost (Just (d5, d2, d0)) = min d5 d2 + d0

zeroPath :: Matrix -> Maybe Path
zeroPath mx = Nothing

minDivisiblePath :: Int -> Matrix -> (Int, Path)
minDivisiblePath n mx = mapSnd reverse $ dpMapMxRD mapper mx ! (rows, cols)
  where
    (rows, cols) = snd $ bounds mx
    mapper lookup ix e = case bi lookup (ix -. 1, ix .- 1) of
      (Nothing, Nothing) -> (dc, "")
      (Just (c, p), Nothing) -> (c, 'R' : p)
      (Nothing, Just (c, p)) -> (c, 'D' : p)
      (Just (c1, p1), Just (c2, p2)) ->
        minBy fst (c1 + dc, 'R' : p1) (c2 + dc, 'D' : p2)
      where
        dc = bit (divides (toInteger n) e)

solveDP :: Matrix -> (Int, Path)
solveDP = undefined

main = do
  dim <- readLn
  mx <- mxRead dim dim
  let (c, p) = solveDP mx
  print c
  putStrLn p

-- At first I've came with this solution, but then realized
-- it's actually not optimal. This is a greedy algorithm, not DP.

solveGreedy :: Matrix -> (Int, Path)
solveGreedy mx = cost *** reverse $ dpMapMxRD mapper mx ! (rows, cols)
  where
    (rows, cols) = snd $ bounds mx
    mapper lookup (r, c) e = case (lookup (r - 1, c), lookup (r, c - 1)) of
      (Nothing, Nothing) -> (initialCost, "")
      (Just (c, p), Nothing) -> (updateCost c, 'D' : p)
      (Nothing, Just (c, p)) -> (updateCost c, 'R' : p)
      (Just (c1, p1), Just (c2, p2)) ->
        minBy (cost . fst) (updateCost c1, 'D' : p1) (updateCost c2, 'R' : p2)
      where
        initialCost = updateCost $ Just (0, 0, 0)
        updateCost Nothing = Nothing
        updateCost (Just c@(d5, d2, d0))
          | e `mod` 10 == 0 = Just (d5, d2, d0 + trailingZeros e)
          | e `mod` 5  == 0 = Just (d5 + 1, d2, d0)
          | e `mod` 2  == 0 = Just (d5, d2 + 1, d0)
          | otherwise       = Just c
