module DP where

import Misc
import Array

import Data.Array.IArray
import Data.Array.MArray
import Data.Array.Unsafe

type Stepper i = i -> Maybe i

type ContextAwareMapper i a b =
  (i -> Maybe b) -> i -> a -> b

dpMap :: (Show i, Ix i, IArray ar a) => ContextAwareMapper i a b -> Stepper i -> i -> ar i a -> Array i b
dpMap mapper stepper ix arr = io $ do
  let (bmin, bmax) = bounds arr
  buff <- mkIOArray (bmin, bmax) Nothing
  arr' <- mkIOArray (bmin, bmax) undefined
  let
    lookup ix
      | ix < bmin || ix > bmax = Nothing
      | otherwise = io $ readArray buff ix
    loop ix = do
      let e' = mapper lookup ix (arr ! ix)
      writeArray arr' ix e'
      writeArray buff ix (Just e')
      maybe nop loop (stepper ix)
  loop ix
  unsafeFreeze arr'

dpMapMxRD :: IArray ar a => ContextAwareMapper Int2 a b -> ar Int2 a -> Array Int2 b
dpMapMxRD mapper arr = dpMap mapper' stepper (1, 1) arr
  where
    (rows, cols) = snd $ bounds arr
    mapper' stdLookup = mapper mxLookup where
      mxLookup (r, c)
        | r < 1 || c < 1 = Nothing
        | otherwise = stdLookup (r, c)
    stepper (i, j)
      | i < rows  = Just (i+1, j)
      | j < cols  = Just (1, j+1)
      | otherwise = Nothing

{- No idea how to make this with ST even in unsafe way:
   ST is extremely hard in such non-trivial scenarios...

mkSTArray :: Ix i => (i, i) -> e -> ST s (STArray s i e)
mkSTArray = newArray

unsafePerformST :: ST s a -> a
unsafePerformST = unsafePerformIO . unsafeSTToIO

dpMap :: (Ix i, IArray a e1)
      => ((i -> Maybe e2) -> i -> e1 -> e2)
      -> (i -> Maybe i) -> i
      -> a i e1 -> Array i e2
dpMap f ixf ix arr = runSTArray $ do
  let bounds' = bounds arr
  buff <- mkSTArray bounds' Nothing
  buffAsArray <- unsafeFreeze buff
  arr' <- mkSTArray bounds' undefined
  let loop ix = do
        let e' = f (buffAsArray !) ix (arr ! ix)
        writeArray arr' ix e'
        writeArray buff ix (Just e')
        maybe nop loop (ixf ix)
  loop ix
  return arr'
-}
