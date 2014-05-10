{-# LANGUAGE FlexibleInstances, FlexibleContexts, FunctionalDependencies #-}

module Indexed where

import Misc

import Data.List (find)
import Data.Array (Array)
import Data.Array.Unboxed (UArray)
import Data.Array.IArray as IA
import Data.Array.MArray as MA
import Data.Array.IO (IOArray, IOUArray)

class Ix i => BoundIx i where
  inBounds :: (i, i) -> i -> Bool

class Indexed s i e | s -> i e where
  (!)    :: s -> i -> e
  assocs :: s -> [(i, e)]
  bounds :: s -> (i, i)

instance BoundIx Int where
  inBounds (a, b) = \x ->
    a <= x && x <= b

instance Ix i => BoundIx (i, i) where
  inBounds ((xMin, yMin), (xMax, yMax)) = \(x, y) ->
    xMin <= x && x <= xMax &&
    yMin <= y && y <= yMax

instance Indexed [e] Int e where
  (!) = (!!)
  assocs xs = zip [0..] xs
  bounds xs = (0, length xs - 1)

instance BoundIx i => Indexed (Array i e) i e where
  a ! ix = a IA.! ix
  bounds = IA.bounds
  assocs = IA.assocs

instance (BoundIx i, IArray UArray e) => Indexed (UArray i e) i e where
  a ! ix = a IA.! ix
  bounds = IA.bounds
  assocs = IA.assocs

instance BoundIx i => Indexed (IOArray i e) i e where
  a ! ix = io $ MA.readArray a ix
  bounds = io . MA.getBounds
  assocs = io . MA.getAssocs

instance (BoundIx i, MArray IOUArray e IO) => Indexed (IOUArray i e) i e where
  a ! ix = io $ MA.readArray a ix
  bounds = io . MA.getBounds
  assocs = io . MA.getAssocs

findIx :: Indexed s i e => (e -> Bool) -> s -> Maybe i
findIx p s = fmap fst $ find (p . snd) $ Indexed.assocs s

lowerBound :: Indexed s i e => s -> i
lowerBound = fst . Indexed.bounds

upperBound :: Indexed s i e => s -> i
upperBound = snd . Indexed.bounds
