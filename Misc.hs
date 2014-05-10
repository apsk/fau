{-# LANGUAGE LambdaCase #-}
module Misc where

import System.IO.Unsafe
import Unsafe.Coerce
import Debug.Trace

type Pair a = (a, a)

nop :: Monad m => m ()
nop = return ()

io :: IO a -> a
io = unsafePerformIO

dbg :: Show a => a -> b -> b
dbg = traceShow

-- Change this to http://hackage.haskell.org/package/base-4.7.0.0/docs/Data-Coerce.html
-- when codeforces will move to GHC 7.8
coerce :: a -> b
coerce = unsafeCoerce

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

type Stepper i =
  i -> Maybe i

mxRD :: Integral i => i -> i -> Stepper (i, i)
mxRD rows cols (i, j)
  | j < cols  = Just (i, j + 1)
  | i < rows  = Just (i + 1, 1)
  | otherwise = Nothing

mb :: b -> (a -> b) -> Maybe a -> b
mb = maybe

mb2 :: c -> (a -> c) -> (b -> c) -> (a -> b -> c) -> (Maybe a, Maybe b) -> c
mb2 x fL fR fB = \case
  (Nothing, Nothing) -> x
  (Just  x, Nothing) -> fL x
  (Nothing, Just  x) -> fR x
  (Just  x, Just  y) -> fB x y

mb2u :: c -> (a -> c) -> (b -> c) -> ((a, b) -> c) -> (Maybe a, Maybe b) -> c
mb2u x fL fR fB = mb2 x fL fR (curry fB)

(.%) :: (b1 -> b2 -> c) -> (a -> (b1, b2)) -> a -> c
(f .% g) x = f $% g x

($%) :: (a -> b -> c) -> (a, b) -> c
f $% (x, y) = f x y
