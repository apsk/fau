module Misc where

import System.IO.Unsafe

type Int2 = (Int, Int)

nop :: Monad m => m ()
nop = return ()

io :: IO a -> a
io = unsafePerformIO

trailingZeros :: Integral a => a -> Int
trailingZeros = length . takeWhile (\x -> x /= 0 && x `mod` 10 == 0) . iterate (`div` 10)

minBy :: Ord b => (a -> b) -> a -> a -> a
minBy f a b
  | f b > f a = a
  | otherwise = b

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)
