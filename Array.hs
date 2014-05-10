{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}

module Array (
  IOUArrayElem,
  IArray,
  MArray,
  Array,
  UArray,
  mkIOArray,
  mkIOUArray,
  mxReadIOU,
  mxReadU
) where

import Data.Array.IO
import Data.Array.Unboxed hiding (bounds)
import Data.Array.Unsafe

import Control.Monad

type IOUArrayElem e = MArray IOUArray e IO

mkIOArray :: Ix i => (i, i) -> e -> IO (IOArray i e)
mkIOArray = newArray

mkIOUArray :: (Ix i, IOUArrayElem e) => (i, i) -> e -> IO (IOUArray i e)
mkIOUArray = newArray

mxReadIOU :: (Read e, IOUArrayElem e) => Int -> Int -> IO (IOUArray (Int, Int) e)
mxReadIOU rows cols = do
  mx <- newArray_ ((1, 1), (rows, cols))
  forM_ [1..rows] $ \i -> do
    line <- getLine
    let els = map read (words line)
    forM_ (zip [1..] els) $ \(j, e) ->
      writeArray mx (i, j) e
  return mx

mxReadU :: (Read e, IArray UArray e, IOUArrayElem e) => Int -> Int -> IO (UArray (Int, Int) e)
mxReadU rows cols = mxReadIOU rows cols >>= unsafeFreeze
