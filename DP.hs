module DP where

import Misc
import Array
import Indexed

import Data.Array.IArray as IA
import Data.Array.MArray as MA
import Data.Array.Unboxed
import Data.Array.Unsafe
import Data.Array.IO

type Mapper i a b =
  (i -> b) -> i -> a -> b

type ChangeAwareMapper i a b =
  (i -> Maybe b) -> i -> a -> b

dpMap :: (BoundIx i, IArray ar1 a, IArray ar2 b) =>
  Mapper i a b -> Stepper i -> i -> b -> ar1 i a -> ar2 i b

dpMap mapper stepper start_ix special_el ar = io $ do
  ar' <- mkIOArray (IA.bounds ar) special_el
  bounds <- MA.getBounds ar'
  let
    validIx = inBounds bounds
    lookup ix
      | not (validIx ix) = special_el
      | otherwise = io $ readArray ar' ix
    loop ix = do
      let e' = mapper lookup ix (ar IA.! ix)
      writeArray ar' ix e'
      maybe nop loop (stepper ix)
  loop start_ix
  unsafeFreeze ar'

dpChangeAwareMap :: (BoundIx i, IArray ar1 a, IArray ar2 b) =>
  ChangeAwareMapper i a b -> Stepper i -> i -> ar1 i a -> ar2 i b

dpChangeAwareMap mapper stepper ix ar = io $ do
  let bounds = IA.bounds ar
  buf <- mkIOArray bounds Nothing
  ar' <- mkIOArray bounds undefined
  let
    validIx = inBounds bounds
    lookup ix
      | not (validIx ix) = Nothing
      | otherwise = io $ readArray buf ix
    loop ix = do
      let e' = mapper lookup ix (ar IA.! ix)
      writeArray ar' ix e'
      writeArray buf ix (Just e')
      maybe nop loop (stepper ix)
  loop ix
  unsafeFreeze ar'
