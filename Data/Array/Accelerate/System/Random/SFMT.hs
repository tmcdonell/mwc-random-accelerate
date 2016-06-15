{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}
-- |
-- Module:      : Data.Array.Accelerate.System.Random.SFMT
-- Copyright    : [2014..2015] Trevor L. McDonell
-- License      : BSD3
--
-- Maintainer   : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)
--
-- Random number generation backed by SMFT.
--

module Data.Array.Accelerate.System.Random.SFMT (

  -- * Generating random arrays
  (:~>),
  uniform, uniformR,
  randomArray, randomArrayWith,

  -- * Re-export SFMT-Random
  module System.Random.SFMT,

) where

import System.Random.SFMT                       hiding ( uniform, uniformR )
import qualified System.Random.SFMT             as R

import Data.Array.Accelerate                    as A
import Data.Array.Accelerate.Array.Data         as A
import Data.Array.Accelerate.Array.Sugar        as Sugar


-- | A PRNG from indices to variates
--
type sh :~> e = sh -> GenIO -> IO e


-- | Uniformly distributed random variates.
--
{-# INLINE uniform #-}
uniform :: (Shape sh, Elt e, Variate e) => sh :~> e
uniform _ = R.uniform

-- | Uniformly distributed random variates in a given range.
--
{-# INLINE uniformR #-}
uniformR :: (Shape sh, Elt e, Variate e) => (e, e) -> sh :~> e
uniformR bounds _ = R.uniformR bounds


-- | Generate an array of random values. The generator for variates is
-- initialised with a fixed seed.
--
{-# INLINE randomArray #-}
randomArray :: (Shape sh, Elt e) => sh :~> e -> sh -> IO (Array sh e)
randomArray f sh
  = do
      gen <- create
      randomArrayWith gen f sh


-- | Generate an array of random values using the supplied generator.
--
{-# INLINE randomArrayWith #-}
randomArrayWith
    :: (Shape sh, Elt e)
    => GenIO
    -> sh :~> e
    -> sh
    -> IO (Array sh e)
randomArrayWith gen f sh
  = do
      adata  <- runRandomArray f sh gen
      return $! Array (fromElt sh) adata


-- Create a mutable array and fill it with random values
--
{-# INLINE runRandomArray #-}
runRandomArray
    :: (Shape sh, Elt e)
    => sh :~> e
    -> sh
    -> GenIO
    -> IO (MutableArrayData (EltRepr e))
runRandomArray f sh gen
  = do
      arr <- newArrayData $! Sugar.size sh
      let !n            = Sugar.size sh
          write !i
            | i >= n    = return ()
            | otherwise = do
                unsafeWriteArrayData arr i . fromElt =<< f (Sugar.fromIndex sh i) gen
                write (i+1)
      --
      write 0
      return arr

