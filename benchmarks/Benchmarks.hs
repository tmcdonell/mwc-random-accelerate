{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Main where

import Data.Array.Accelerate
import Data.Array.Accelerate.System.Random.MWC          as AMWC
-- import Data.Array.Accelerate.System.Random.SFMT         as ASFMT
import System.Random.MWC                                as MWC
import System.Random.SFMT                               as SFMT
import System.Random.Mersenne                           as MT
import Data.Vector.Unboxed                              as U

import Criterion.Main
import Criterion.Types
import Data.Proxy
import Prelude                                          as P


makeMWCArray :: forall e. Elt e => MWC.GenIO -> DIM1 AMWC.:~> e -> Benchmarkable
makeMWCArray mwc f = Benchmarkable arr
  where
    arr :: Int64 -> IO ()
    arr n = do
      !_ <- AMWC.randomArrayWith mwc f (Z :. P.fromIntegral n) :: IO (Array DIM1 e)
      return ()

-- makeSFMTArray :: forall e. Elt e => SFMT.GenIO -> DIM1 ASFMT.:~> e -> Benchmarkable
-- makeSFMTArray sfmt f = Benchmarkable arr
--   where
--     arr :: Int64 -> IO ()
--     arr n = do
--       !_ <- ASFMT.randomArrayWith sfmt f (Z :. P.fromIntegral n) :: IO (Array DIM1 e)
--       return ()

makeVector :: forall e. (MWC.Variate e, Unbox e) => MWC.GenIO -> Proxy e -> Benchmarkable
makeVector mwc _ = Benchmarkable vec
  where
    vec :: Int64 -> IO ()
    vec n = do
      !_ <- MWC.uniformVector mwc (P.fromIntegral n) :: IO (U.Vector e)
      return ()


main :: IO ()
main = do
  mwc  <- MWC.create
  sfmt <- SFMT.create
  mt   <- MT.getStdGen

  defaultMain
    -- One letter group names are used so they will fit on the plot.
    --
    --  U - uniform
    --  R - uniformR
    --  D - distribution
    --
    [ bgroup "mersenne-random"
      [ bgroup "U"
        [ bench "Double"  (whnfIO (MT.random mt :: IO Double))
        , bench "Int"     (whnfIO (MT.random mt :: IO Int))
        , bench "Int8"    (whnfIO (MT.random mt :: IO Int8))
        , bench "Int16"   (whnfIO (MT.random mt :: IO Int16))
        , bench "Int32"   (whnfIO (MT.random mt :: IO Int32))
        , bench "Int64"   (whnfIO (MT.random mt :: IO Int64))
        , bench "Word"    (whnfIO (MT.random mt :: IO Word))
        , bench "Word8"   (whnfIO (MT.random mt :: IO Word8))
        , bench "Word16"  (whnfIO (MT.random mt :: IO Word16))
        , bench "Word32"  (whnfIO (MT.random mt :: IO Word32))
        , bench "Word64"  (whnfIO (MT.random mt :: IO Word64))
        ]
      ]

    -- stolen from MWC random benchmark suite
    , bgroup "mwc-random"
      [ bgroup "U"
        [ bench "Double"  (whnfIO (MWC.uniform mwc :: IO Double))
        , bench "Float"   (whnfIO (MWC.uniform mwc :: IO Float))
        , bench "Int"     (whnfIO (MWC.uniform mwc :: IO Int))
        , bench "Int8"    (whnfIO (MWC.uniform mwc :: IO Int8))
        , bench "Int16"   (whnfIO (MWC.uniform mwc :: IO Int16))
        , bench "Int32"   (whnfIO (MWC.uniform mwc :: IO Int32))
        , bench "Int64"   (whnfIO (MWC.uniform mwc :: IO Int64))
        , bench "Word"    (whnfIO (MWC.uniform mwc :: IO Word))
        , bench "Word8"   (whnfIO (MWC.uniform mwc :: IO Word8))
        , bench "Word16"  (whnfIO (MWC.uniform mwc :: IO Word16))
        , bench "Word32"  (whnfIO (MWC.uniform mwc :: IO Word32))
        , bench "Word64"  (whnfIO (MWC.uniform mwc :: IO Word64))
        ]
      , bgroup "R"
        -- I'm not entirely convinced that this is right way to test
        -- uniformR. /A.Khudyakov/
        [ bench "Double"  (whnfIO (MWC.uniformR (-3.21,26) mwc :: IO Double))
        , bench "Float"   (whnfIO (MWC.uniformR (-3.21,26) mwc :: IO Float))
        , bench "Int"     (whnfIO (MWC.uniformR (-12,679)  mwc :: IO Int))
        , bench "Int8"    (whnfIO (MWC.uniformR (-12,4)    mwc :: IO Int8))
        , bench "Int16"   (whnfIO (MWC.uniformR (-12,679)  mwc :: IO Int16))
        , bench "Int32"   (whnfIO (MWC.uniformR (-12,679)  mwc :: IO Int32))
        , bench "Int64"   (whnfIO (MWC.uniformR (-12,679)  mwc :: IO Int64))
        , bench "Word"    (whnfIO (MWC.uniformR (34,633)   mwc :: IO Word))
        , bench "Word8"   (whnfIO (MWC.uniformR (34,63)    mwc :: IO Word8))
        , bench "Word16"  (whnfIO (MWC.uniformR (34,633)   mwc :: IO Word16))
        , bench "Word32"  (whnfIO (MWC.uniformR (34,633)   mwc :: IO Word32))
        , bench "Word64"  (whnfIO (MWC.uniformR (34,633)   mwc :: IO Word64))
        ]
      , bgroup "vector"
        [ bgroup "U"
          [ bench "Double"  (makeVector mwc (Proxy :: Proxy Double))
          , bench "Float"   (makeVector mwc (Proxy :: Proxy Float))
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
      , bgroup "Acc"
        [ bgroup "U"
          [ bench "Double"  (makeMWCArray mwc (AMWC.uniform :: DIM1 AMWC.:~> Double))
          , bench "Float"   (makeMWCArray mwc (AMWC.uniform :: DIM1 AMWC.:~> Float))
          , bench "Int"     (makeMWCArray mwc (AMWC.uniform :: DIM1 AMWC.:~> Int))
          , bench "Int8"    (makeMWCArray mwc (AMWC.uniform :: DIM1 AMWC.:~> Int8))
          , bench "Int16"   (makeMWCArray mwc (AMWC.uniform :: DIM1 AMWC.:~> Int16))
          , bench "Int32"   (makeMWCArray mwc (AMWC.uniform :: DIM1 AMWC.:~> Int32))
          , bench "Int64"   (makeMWCArray mwc (AMWC.uniform :: DIM1 AMWC.:~> Int64))
          , bench "Word"    (makeMWCArray mwc (AMWC.uniform :: DIM1 AMWC.:~> Word))
          , bench "Word8"   (makeMWCArray mwc (AMWC.uniform :: DIM1 AMWC.:~> Word8))
          , bench "Word16"  (makeMWCArray mwc (AMWC.uniform :: DIM1 AMWC.:~> Word16))
          , bench "Word32"  (makeMWCArray mwc (AMWC.uniform :: DIM1 AMWC.:~> Word32))
          , bench "Word64"  (makeMWCArray mwc (AMWC.uniform :: DIM1 AMWC.:~> Word64))
          ]
        , bgroup "R"
          [ bench "Double"  (makeMWCArray mwc (AMWC.uniformR (-3.21,26) :: DIM1 AMWC.:~> Double))
          , bench "Float"   (makeMWCArray mwc (AMWC.uniformR (-3.21,26) :: DIM1 AMWC.:~> Float))
          , bench "Int"     (makeMWCArray mwc (AMWC.uniformR (-12,679)  :: DIM1 AMWC.:~> Int))
          , bench "Int8"    (makeMWCArray mwc (AMWC.uniformR (-12,4)    :: DIM1 AMWC.:~> Int8))
          , bench "Int16"   (makeMWCArray mwc (AMWC.uniformR (-12,679)  :: DIM1 AMWC.:~> Int16))
          , bench "Int32"   (makeMWCArray mwc (AMWC.uniformR (-12,679)  :: DIM1 AMWC.:~> Int32))
          , bench "Int64"   (makeMWCArray mwc (AMWC.uniformR (-12,679)  :: DIM1 AMWC.:~> Int64))
          , bench "Word"    (makeMWCArray mwc (AMWC.uniformR (34,633)   :: DIM1 AMWC.:~> Word))
          , bench "Word8"   (makeMWCArray mwc (AMWC.uniformR (34,63)    :: DIM1 AMWC.:~> Word8))
          , bench "Word16"  (makeMWCArray mwc (AMWC.uniformR (34,633)   :: DIM1 AMWC.:~> Word16))
          , bench "Word32"  (makeMWCArray mwc (AMWC.uniformR (34,633)   :: DIM1 AMWC.:~> Word32))
          , bench "Word64"  (makeMWCArray mwc (AMWC.uniformR (34,633)   :: DIM1 AMWC.:~> Word64))
          ]
        ]
      ]

{--
    -- SFMT
    , bgroup "sfmt"
      [ bgroup "U"
        [ bench "Double"  (whnfIO (SFMT.uniform sfmt :: IO Double))
        , bench "Float"   (whnfIO (SFMT.uniform sfmt :: IO Float))
        , bench "Int"     (whnfIO (SFMT.uniform sfmt :: IO Int))
        , bench "Int8"    (whnfIO (SFMT.uniform sfmt :: IO Int8))
        , bench "Int16"   (whnfIO (SFMT.uniform sfmt :: IO Int16))
        , bench "Int32"   (whnfIO (SFMT.uniform sfmt :: IO Int32))
        , bench "Int64"   (whnfIO (SFMT.uniform sfmt :: IO Int64))
        , bench "Word"    (whnfIO (SFMT.uniform sfmt :: IO Word))
        , bench "Word8"   (whnfIO (SFMT.uniform sfmt :: IO Word8))
        , bench "Word16"  (whnfIO (SFMT.uniform sfmt :: IO Word16))
        , bench "Word32"  (whnfIO (SFMT.uniform sfmt :: IO Word32))
        , bench "Word64"  (whnfIO (SFMT.uniform sfmt :: IO Word64))
        ]
      , bgroup "R"
        [ bench "Double"  (whnfIO (SFMT.uniformR (-3.21,26) sfmt :: IO Double))
        , bench "Float"   (whnfIO (SFMT.uniformR (-3.21,26) sfmt :: IO Float))
        , bench "Int"     (whnfIO (SFMT.uniformR (-12,679)  sfmt :: IO Int))
        , bench "Int8"    (whnfIO (SFMT.uniformR (-12,4)    sfmt :: IO Int8))
        , bench "Int16"   (whnfIO (SFMT.uniformR (-12,679)  sfmt :: IO Int16))
        , bench "Int32"   (whnfIO (SFMT.uniformR (-12,679)  sfmt :: IO Int32))
        , bench "Int64"   (whnfIO (SFMT.uniformR (-12,679)  sfmt :: IO Int64))
        , bench "Word"    (whnfIO (SFMT.uniformR (34,633)   sfmt :: IO Word))
        , bench "Word8"   (whnfIO (SFMT.uniformR (34,63)    sfmt :: IO Word8))
        , bench "Word16"  (whnfIO (SFMT.uniformR (34,633)   sfmt :: IO Word16))
        , bench "Word32"  (whnfIO (SFMT.uniformR (34,633)   sfmt :: IO Word32))
        , bench "Word64"  (whnfIO (SFMT.uniformR (34,633)   sfmt :: IO Word64))
        ]
      , bgroup "Acc"
        [ bgroup "U"
          [ bench "Double"  (makeSFMTArray sfmt (ASFMT.uniform :: DIM1 ASFMT.:~> Double))
          , bench "Float"   (makeSFMTArray sfmt (ASFMT.uniform :: DIM1 ASFMT.:~> Float))
          , bench "Int"     (makeSFMTArray sfmt (ASFMT.uniform :: DIM1 ASFMT.:~> Int))
          , bench "Int8"    (makeSFMTArray sfmt (ASFMT.uniform :: DIM1 ASFMT.:~> Int8))
          , bench "Int16"   (makeSFMTArray sfmt (ASFMT.uniform :: DIM1 ASFMT.:~> Int16))
          , bench "Int32"   (makeSFMTArray sfmt (ASFMT.uniform :: DIM1 ASFMT.:~> Int32))
          , bench "Int64"   (makeSFMTArray sfmt (ASFMT.uniform :: DIM1 ASFMT.:~> Int64))
          , bench "Word"    (makeSFMTArray sfmt (ASFMT.uniform :: DIM1 ASFMT.:~> Word))
          , bench "Word8"   (makeSFMTArray sfmt (ASFMT.uniform :: DIM1 ASFMT.:~> Word8))
          , bench "Word16"  (makeSFMTArray sfmt (ASFMT.uniform :: DIM1 ASFMT.:~> Word16))
          , bench "Word32"  (makeSFMTArray sfmt (ASFMT.uniform :: DIM1 ASFMT.:~> Word32))
          , bench "Word64"  (makeSFMTArray sfmt (ASFMT.uniform :: DIM1 ASFMT.:~> Word64))
          ]
        , bgroup "R"
          [ bench "Double"  (makeSFMTArray sfmt (ASFMT.uniformR (-3.21,26) :: DIM1 ASFMT.:~> Double))
          , bench "Float"   (makeSFMTArray sfmt (ASFMT.uniformR (-3.21,26) :: DIM1 ASFMT.:~> Float))
          , bench "Int"     (makeSFMTArray sfmt (ASFMT.uniformR (-12,679)  :: DIM1 ASFMT.:~> Int))
          , bench "Int8"    (makeSFMTArray sfmt (ASFMT.uniformR (-12,4)    :: DIM1 ASFMT.:~> Int8))
          , bench "Int16"   (makeSFMTArray sfmt (ASFMT.uniformR (-12,679)  :: DIM1 ASFMT.:~> Int16))
          , bench "Int32"   (makeSFMTArray sfmt (ASFMT.uniformR (-12,679)  :: DIM1 ASFMT.:~> Int32))
          , bench "Int64"   (makeSFMTArray sfmt (ASFMT.uniformR (-12,679)  :: DIM1 ASFMT.:~> Int64))
          , bench "Word"    (makeSFMTArray sfmt (ASFMT.uniformR (34,633)   :: DIM1 ASFMT.:~> Word))
          , bench "Word8"   (makeSFMTArray sfmt (ASFMT.uniformR (34,63)    :: DIM1 ASFMT.:~> Word8))
          , bench "Word16"  (makeSFMTArray sfmt (ASFMT.uniformR (34,633)   :: DIM1 ASFMT.:~> Word16))
          , bench "Word32"  (makeSFMTArray sfmt (ASFMT.uniformR (34,633)   :: DIM1 ASFMT.:~> Word32))
          , bench "Word64"  (makeSFMTArray sfmt (ASFMT.uniformR (34,633)   :: DIM1 ASFMT.:~> Word64))
          ]
        ]
      ]
--}
    ]

