module CF_P2B where -- http://codeforces.ru/problemset/problem/2/B

import Data.Word
import Data.List
import Control.Arrow
import Control.Applicative

import Array
import Indexed
import Numeric
import Misc
import DP

type Number = Word32
type Count = Int
type Matrix = UArray (Int, Int) Number
type Path = String

zeroPath :: Matrix -> Maybe Path
zeroPath mx = pathThrough <$> findIx is0 mx
  where
    (r, c) = upperBound mx

    pathThrough (ri, ci) = replicate (ci - 1) 'R' ++
      replicate (r - 1) 'D' ++ replicate (c - ci) 'R'

minDivisiblePath :: Number -> Matrix -> (Count, Path)
minDivisiblePath n mx = mapSnd reverse (dpMx ! (r, c))
  where
    (r, c) = upperBound mx

    dpMx :: Array (Int, Int) (Count, Path)
    dpMx = dpChangeAwareMap mapper (mxRD r c) (1, 1) mx

    mapper lookup ix e =
      mb2u (dc, "") fR fD (minBy fst .% (fR *** fD)) $
        bi lookup (ix -. 1, ix .- 1)
      where
        dc = timesDivisibleBy (fi n) e
        fR = (dc +) *** ('R' :)
        fD = (dc +) *** ('D' :)

solveDP :: Matrix -> (Count, Path)
solveDP mx = maybe cp25 (\p0 -> minBy fst cp25 (1, p0)) (zeroPath mx)
  where
    cp25 = minBy fst $% bi (flip minDivisiblePath mx) (2, 5)

main = do
  dim <- readLn
  mx <- mxReadU dim dim
  let (c, p) = solveDP mx
  print c
  putStrLn p

-- At first I've came with this solution, but then realized
-- it's actually not optimal. This is a greedy algorithm, not DP:

{-
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
-}
