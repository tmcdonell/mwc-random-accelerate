{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Main where

import Data.Array.Accelerate
import Data.Array.Accelerate.System.Random.MWC          as A
import System.Random.MWC                                as R
import Data.Vector.Unboxed                              as U

import Criterion.Main
import Criterion.Types
import Data.Proxy
import Prelude                                          as P


makeArray :: forall e. Elt e => GenIO -> DIM1 :~> e -> Benchmarkable
makeArray mwc f = Benchmarkable arr
  where
    arr :: Int64 -> IO ()
    arr n = do
      !_ <- randomArrayWith mwc f (Z :. P.fromIntegral n) :: IO (Array DIM1 e)
      return ()

makeVector :: forall e. (Variate e, Unbox e) => GenIO -> Proxy e -> Benchmarkable
makeVector mwc _ = Benchmarkable vec
  where
    vec :: Int64 -> IO ()
    vec n = do
      !_ <- uniformVector mwc (P.fromIntegral n) :: IO (U.Vector e)
      return ()


main :: IO ()
main = do
  mwc <- R.create

  defaultMain
    -- stolen from MWC random benchmark suite
    [ bgroup "mwc"
      -- One letter group names are used so they will fit on the plot.
      --
      --  U - uniform
      --  R - uniformR
      --  D - distribution
      [ bgroup "U"
        [ bench "Double"  (whnfIO (R.uniform mwc :: IO Double))
        , bench "Int"     (whnfIO (R.uniform mwc :: IO Int))
        , bench "Int8"    (whnfIO (R.uniform mwc :: IO Int8))
        , bench "Int16"   (whnfIO (R.uniform mwc :: IO Int16))
        , bench "Int32"   (whnfIO (R.uniform mwc :: IO Int32))
        , bench "Int64"   (whnfIO (R.uniform mwc :: IO Int64))
        , bench "Word"    (whnfIO (R.uniform mwc :: IO Word))
        , bench "Word8"   (whnfIO (R.uniform mwc :: IO Word8))
        , bench "Word16"  (whnfIO (R.uniform mwc :: IO Word16))
        , bench "Word32"  (whnfIO (R.uniform mwc :: IO Word32))
        , bench "Word64"  (whnfIO (R.uniform mwc :: IO Word64))
        ]
      , bgroup "R"
        -- I'm not entirely convinced that this is right way to test
        -- uniformR. /A.Khudyakov/
        [ bench "Double"  (whnfIO (R.uniformR (-3.21,26) mwc :: IO Double))
        , bench "Int"     (whnfIO (R.uniformR (-12,679)  mwc :: IO Int))
        , bench "Int8"    (whnfIO (R.uniformR (-12,4)    mwc :: IO Int8))
        , bench "Int16"   (whnfIO (R.uniformR (-12,679)  mwc :: IO Int16))
        , bench "Int32"   (whnfIO (R.uniformR (-12,679)  mwc :: IO Int32))
        , bench "Int64"   (whnfIO (R.uniformR (-12,679)  mwc :: IO Int64))
        , bench "Word"    (whnfIO (R.uniformR (34,633)   mwc :: IO Word))
        , bench "Word8"   (whnfIO (R.uniformR (34,63)    mwc :: IO Word8))
        , bench "Word16"  (whnfIO (R.uniformR (34,633)   mwc :: IO Word16))
        , bench "Word32"  (whnfIO (R.uniformR (34,633)   mwc :: IO Word32))
        , bench "Word64"  (whnfIO (R.uniformR (34,633)   mwc :: IO Word64))
        ]
      ]

    , bgroup "acc"
      [ bgroup "U"
        [ bench "Double"  (makeArray mwc (A.uniform :: DIM1 :~> Double))
        , bench "Int"     (makeArray mwc (A.uniform :: DIM1 :~> Int))
        , bench "Int8"    (makeArray mwc (A.uniform :: DIM1 :~> Int8))
        , bench "Int16"   (makeArray mwc (A.uniform :: DIM1 :~> Int16))
        , bench "Int32"   (makeArray mwc (A.uniform :: DIM1 :~> Int32))
        , bench "Int64"   (makeArray mwc (A.uniform :: DIM1 :~> Int64))
        , bench "Word"    (makeArray mwc (A.uniform :: DIM1 :~> Word))
        , bench "Word8"   (makeArray mwc (A.uniform :: DIM1 :~> Word8))
        , bench "Word16"  (makeArray mwc (A.uniform :: DIM1 :~> Word16))
        , bench "Word32"  (makeArray mwc (A.uniform :: DIM1 :~> Word32))
        , bench "Word64"  (makeArray mwc (A.uniform :: DIM1 :~> Word64))
        ]
      , bgroup "R"
        [ bench "Double"  (makeArray mwc (A.uniformR (-3.21,26) :: DIM1 :~> Double))
        , bench "Int"     (makeArray mwc (A.uniformR (-12,679)  :: DIM1 :~> Int))
        , bench "Int8"    (makeArray mwc (A.uniformR (-12,4)    :: DIM1 :~> Int8))
        , bench "Int16"   (makeArray mwc (A.uniformR (-12,679)  :: DIM1 :~> Int16))
        , bench "Int32"   (makeArray mwc (A.uniformR (-12,679)  :: DIM1 :~> Int32))
        , bench "Int64"   (makeArray mwc (A.uniformR (-12,679)  :: DIM1 :~> Int64))
        , bench "Word"    (makeArray mwc (A.uniformR (34,633)   :: DIM1 :~> Word))
        , bench "Word8"   (makeArray mwc (A.uniformR (34,63)    :: DIM1 :~> Word8))
        , bench "Word16"  (makeArray mwc (A.uniformR (34,633)   :: DIM1 :~> Word16))
        , bench "Word32"  (makeArray mwc (A.uniformR (34,633)   :: DIM1 :~> Word32))
        , bench "Word64"  (makeArray mwc (A.uniformR (34,633)   :: DIM1 :~> Word64))
        ]
      ]

    , bgroup "vector"
      [ bgroup "U"
        [ bench "Double"  (makeVector mwc (Proxy :: Proxy Double))
        , bench "Int"     (makeVector mwc (Proxy :: Proxy Int))
        , bench "Int8"    (makeVector mwc (Proxy :: Proxy Int8))
        , bench "Int16"   (makeVector mwc (Proxy :: Proxy Int16))
        , bench "Int32"   (makeVector mwc (Proxy :: Proxy Int32))
        , bench "Int64"   (makeVector mwc (Proxy :: Proxy Int64))
        , bench "Word"    (makeVector mwc (Proxy :: Proxy Word))
        , bench "Word8"   (makeVector mwc (Proxy :: Proxy Word8))
        , bench "Word16"  (makeVector mwc (Proxy :: Proxy Word16))
        , bench "Word32"  (makeVector mwc (Proxy :: Proxy Word32))
        , bench "Word64"  (makeVector mwc (Proxy :: Proxy Word64))
        ]
      ]
    ]
