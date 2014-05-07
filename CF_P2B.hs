module CF_P2B where -- http://codeforces.ru/problemset/problem/2/B

import Data.List
import Data.Array
import Control.Arrow

import Array
import Misc
import DP

type Path = String
type Matrix = Array Int2 Integer

zeroPath :: Matrix -> Maybe Path
zeroPath mx = fmap (pathThrough . fst) $ find ((0 ==) . snd) $ assocs mx
  where
    (rows, cols) = snd $ bounds mx
    pathThrough (r, c) =
      replicate (c - 1) 'R' ++
      replicate (rows - 1) 'D' ++
      replicate (cols - c) 'R'

minDivisiblePath :: Matrix -> Int -> (Int, Path)
minDivisiblePath mx n = mapSnd reverse $ dpMapMxRD mapper mx ! (rows, cols)
  where
    (rows, cols) = snd $ bounds mx
    mapper lookup ix e = case bi lookup (ix -. 1, ix .- 1) of
      (Nothing, Nothing) -> (dc, "")
      (Just (c, p), Nothing) -> (c + dc, 'R' : p)
      (Nothing, Just (c, p)) -> (c + dc, 'D' : p)
      (Just (c1, p1), Just (c2, p2)) ->
        minBy fst (c1 + dc, 'R' : p1) (c2 + dc, 'D' : p2)
      where
        dc = timesDivisibleBy (toi n) e

solveDP :: Matrix -> (Int, Path)
solveDP mx = maybe cp25 (\p0 -> minBy fst cp25 (1, p0)) (zeroPath mx)
  where
    cp25 = uncurry (minBy fst) $ bi (minDivisiblePath mx) (2, 5)

main = do
  dim <- readLn
  mx <- mxRead dim dim
  let (c, p) = solveDP mx
  print c
  putStrLn p

-- At first I've came with this solution, but then realized
-- it's actually not optimal. This is a greedy algorithm, not DP.

cost Nothing             = 0
cost (Just (d5, d2, d0)) = min d5 d2 + d0

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
          | e `mod` 10 == 0 = Just (d5, d2, d0 + timesDivisibleBy 10 e)
          | e `mod` 5  == 0 = Just (d5 + 1, d2, d0)
          | e `mod` 2  == 0 = Just (d5, d2 + 1, d0)
          | otherwise       = Just c
