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

bit :: Integral a => Bool -> a
bit True  = 1
bit False = 0

toi :: Integral a => a -> Integer
toi = toInteger

divides :: Integral a => a -> a -> Bool
divides d x = x `mod` d == 0

timesDivisibleBy :: Integral a => a -> a -> Int
timesDivisibleBy d = length . takeWhile (\r -> r `mod` d == 0) . iterate (`div` d)

(x, y) .- z = (x - z, y)
(x, y) -. z = (x, y - z)
(x, y) .+ z = (x + z, y)
(x, y) +. z = (x, y + z)
(x, y) .* z = (x * z, y)
(x, y) *. z = (x, y * z)
(x, y) ./ z = (x / z, y)
(x, y) /. z = (x, y / z)

bi :: (a -> b) -> (a, a) -> (b, b)
bi f (a, b) = (f a, f b)

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

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)
