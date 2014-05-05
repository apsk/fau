module Misc where

import System.IO.Unsafe
import Debug.Trace

type Int2 = (Int, Int)

nop :: Monad m => m ()
nop = return ()

io :: IO a -> a
io = unsafePerformIO

dbg :: Show a => a -> b -> b
dbg = traceShow

trailingZeros :: Integral a => a -> Int
trailingZeros = length . takeWhile (\x -> x /= 0 && x `mod` 10 == 0) . iterate (`div` 10)

minBy :: Ord b => (a -> b) -> a -> a -> a
minBy f a b
  | f b > f a = a
  | otherwise = b

minByManyOn estimators selector a b = walk estimators
  where
    a' = selector a
    b' = selector b
    walk [] = a
    walk (estimator : estimators) =
      case compare (estimator a') (estimator b') of
        LT -> a
        GT -> b
        EQ -> walk estimators

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)
