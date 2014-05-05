module Array where

import Data.Array.IArray
import Data.Array.IO
import Data.Array.Unsafe

import Control.Monad

mkIOArray :: Ix i => (i, i) -> e -> IO (IOArray i e)
mkIOArray = newArray

mxReadM :: Read e => Int -> Int -> IO (IOArray (Int, Int) e)
mxReadM rows cols = do
  mx <- newArray_ ((1, 1), (rows, cols))
  forM_ [1..rows] $ \i -> do
    line <- getLine
    let els = map read (words line)
    forM_ (zip [1..] els) $ \(j, e) ->
      writeArray mx (i, j) e
  return mx

mxRead :: (IArray a e, Read e) => Int -> Int -> IO (a (Int, Int) e)
mxRead rows cols = mxReadM rows cols >>= unsafeFreeze
