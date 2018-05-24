{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}
-- |
-- Module:      : Data.Array.Accelerate.System.Random.MWC
-- Copyright    : [2014..2015] Trevor L. McDonell
-- License      : BSD3
--
-- Maintainer   : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)
--
-- Random number generation backed by MWC.
--
-- [/Example/]
--
-- Create a vector of 100 random uniformly distributed floating-point numbers,
-- where the PRNG is seeded with data from the system's source of pseudo-random
-- numbers (see 'R.withSystemRandom'):
--
-- >>> vs <- randomArray uniform (Z :. 100)           :: IO (Vector Float)
--
-- To generate uniformly distributed random variables in the range (-1,1]:
--
-- >>> vs <- randomArray (uniformR (-1,1)) (Z:.100)   :: IO (Vector Double)
--
-- You can also pass the generator state in explicitly, so that it can be
-- reused:
--
-- >>> gen <- create                                  :: IO GenIO
-- >>> vs  <- randomArrayWith gen uniform (Z :. 100)  :: IO (Vector Int)
--
-- [/Non-uniform distributions/]
--
-- If you require random numbers following other distributions, you can combine
-- this package with the generators from the
-- <http://hackage.haskell.org/package/random-fu random-fu> package. For
-- example:
--
-- @
-- import Data.Random                                    hiding ( uniform )
-- import qualified Data.Random.Distribution.Exponential as R
-- import qualified Data.Random.Distribution.Poisson     as R
--
-- exponential
--     :: (Distribution StdUniform e, Floating e, Shape sh, Elt e)
--     => e
--     -> sh :~> e
-- exponential beta _sh gen = sampleFrom gen (R.exponential beta)
--
-- poisson
--     :: (Distribution (R.Poisson b) a, Shape sh, Elt a)
--     => b
--     -> sh :~> a
-- poisson lambda _sh gen = sampleFrom gen (R.poisson lambda)
-- @
--
-- Which can then be used as before:
--
-- >>> vs <- randomArray (exponential 5) (Z :. 100)   :: IO (Vector Float)
-- >>> us <- randomArray (poisson 5)     (Z :. 100)   :: IO (Vector Float)
--

module Data.Array.Accelerate.System.Random.MWC (

  -- * Generating random arrays
  (:~>),
  uniform, uniformR,
  randomArray, randomArrayWith,

  -- Re-export MWC-Random
  module System.Random.MWC,

) where

import Prelude                                  as P
import System.Random.MWC                        hiding ( uniform, uniformR )
import qualified System.Random.MWC              as R

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
-- seeded from the system's fast source of pseudo-random numbers (see:
-- 'R.createSystemRandom')
--
{-# INLINEABLE randomArray #-}
randomArray :: (Shape sh, Elt e) => sh :~> e -> sh -> IO (Array sh e)
randomArray f sh
  = do
      gen <- createSystemRandom
      randomArrayWith gen f sh


-- | Generate an array of random values using the supplied generator.
--
{-# INLINEABLE randomArrayWith #-}
randomArrayWith
    :: (Shape sh, Elt e)
    => GenIO
    -> sh :~> e
    -> sh
    -> IO (Array sh e)
randomArrayWith gen f sh
  = do
      adata <- runRandomArray f sh gen
      return $ adata `seq` Array (fromElt sh) adata


-- Create a mutable array and fill it with random values
--
{-# INLINEABLE runRandomArray #-}
runRandomArray
    :: (Shape sh, Elt e)
    => sh :~> e
    -> sh
    -> GenIO
    -> IO (MutableArrayData (EltRepr e))
runRandomArray f sh gen
  = do
      let n = Sugar.size sh
      arr  <- newArrayData n
      --
      let write !i
            | i P.>= n  = return ()
            | otherwise = do
                unsafeWriteArrayData arr i . fromElt =<< f (Sugar.fromIndex sh i) gen
                write (i+1)
      --
      write 0
      return arr

