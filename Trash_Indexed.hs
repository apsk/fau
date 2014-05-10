{-# LANGUAGE FlexibleInstances, FlexibleContexts, FunctionalDependencies #-}

module Indexed where

import Misc

import Data.List (find)
import Data.Array (Array)
import Data.Array.Unboxed (UArray)
import Data.Array.IArray as IA
import Data.Array.MArray as MA
import Data.Array.IO (IOArray, IOUArray)

newtype Ix1 i = Ix1 i      deriving (Ix, Eq, Ord)
newtype Ix2 i = Ix2 (i, i) deriving (Ix, Eq, Ord)

class Indexed s i e | s -> i e where
  (!)      :: s -> i -> e
  assocs   :: s -> [(i, e)]
  bounds   :: s -> (i, i)
  inBounds :: s -> i -> Bool

instance Indexed [e] Int e where
  (!) = (!!)
  assocs xs = zip [0..] xs
  bounds xs = (0, length xs - 1)
  inBounds xs = \ix ->
      0 <= ix && ix < len
    where
      len = length xs

instance Ix i => Indexed (Array (Ix1 i) e) i e where
  a ! ix = a IA.! Ix1 ix
  bounds = coerce . IA.bounds
  assocs = coerce . IA.assocs
  inBounds a = \ix ->
      minIx <= ix && ix <= maxIx
    where
      (minIx, maxIx) = Indexed.bounds a

instance Ix i => Indexed (Array (Ix2 i) e) (i, i) e where
  a ! ix = a IA.! Ix2 ix
  bounds = coerce . IA.bounds
  assocs = coerce . IA.assocs
  inBounds a = \(ri, ci) ->
      ri >= rMin && ci >= cMin &&
      ri <= rMax && ci <= cMax
    where
      ((rMin, cMin), (rMax, cMax)) = Indexed.bounds a

instance (Ix i, IArray UArray e) => Indexed (UArray (Ix1 i) e) i e where
  a ! ix = a IA.! Ix1 ix
  bounds = coerce . IA.bounds
  assocs = coerce . IA.assocs
  inBounds a = \ix ->
      minIx <= ix && ix <= maxIx
    where
      (minIx, maxIx) = Indexed.bounds a

instance (Ix i, IArray UArray e) => Indexed (UArray (Ix2 i) e) (i, i) e where
  a ! ix = a IA.! Ix2 ix
  bounds = coerce . IA.bounds
  assocs = coerce . IA.assocs
  inBounds a = \(ri, ci) ->
      ri >= rMin && ci >= cMin &&
      ri <= rMax && ci <= cMax
    where
      ((rMin, cMin), (rMax, cMax)) = Indexed.bounds a

instance Ix i => Indexed (IOArray (Ix1 i) e) i e where
  a ! ix = io $ MA.readArray a (Ix1 ix)
  bounds = coerce . io . MA.getBounds
  assocs = coerce . io . MA.getAssocs
  inBounds a = \ix ->
      minIx <= ix && ix <= maxIx
    where
      (minIx, maxIx) = Indexed.bounds a

instance Ix i => Indexed (IOArray (Ix2 i) e) (i, i) e where
  a ! ix = io $ MA.readArray a (Ix2 ix)
  bounds = coerce . io . MA.getBounds
  assocs = coerce . io . MA.getAssocs
  inBounds a = \(ri, ci) ->
      ri >= rMin && ci >= cMin &&
      ri <= rMax && ci <= cMax
    where
      ((rMin, cMin), (rMax, cMax)) = Indexed.bounds a

instance (Ix i, MArray IOUArray e IO) => Indexed (IOUArray (Ix1 i) e) i e where
  a ! ix = io $ MA.readArray a (Ix1 ix)
  bounds = coerce . io . MA.getBounds
  assocs = coerce . io . MA.getAssocs
  inBounds a = \ix ->
      minIx <= ix && ix <= maxIx
    where
      (minIx, maxIx) = Indexed.bounds a

instance (Ix i, MArray IOUArray e IO) => Indexed (IOUArray (Ix2 i) e) (i, i) e where
  a ! ix = io $ MA.readArray a (Ix2 ix)
  bounds = coerce . io . MA.getBounds
  assocs = coerce . io . MA.getAssocs
  inBounds a = \(ri, ci) ->
      ri >= rMin && ci >= cMin &&
      ri <= rMax && ci <= cMax
    where
      ((rMin, cMin), (rMax, cMax)) = Indexed.bounds a
