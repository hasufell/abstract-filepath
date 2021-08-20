-- |
-- Copyright   : (c) 2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash           #-}

module Short.BenchAll (benchmarks) where

import Short.BenchIndices

import           Data.Monoid
import           Data.String
import           Test.Tasty.Bench
import           Prelude                               hiding (words)

import qualified Data.ByteString.Char8                 as S8
import           Data.ByteString.Short                 ( ShortByteString )
import qualified AFP.Data.ByteString.Short             as S


import           Foreign

import System.Random



------------------------------------------------------------------------------
-- Benchmark
------------------------------------------------------------------------------

-- input data (NOINLINE to ensure memoization)
----------------------------------------------

-- | Few-enough repetitions to avoid making GC too expensive.
nRepl :: Int
nRepl = 10000

{-# NOINLINE intData #-}
intData :: [Int]
intData = [1..nRepl]

{-# NOINLINE byteStringData #-}
byteStringData :: ShortByteString
byteStringData = S.pack $ map fromIntegral intData

-- benchmark wrappers
---------------------

{-# INLINE benchB' #-}
benchB' :: String -> a -> (a -> ShortByteString) -> Benchmark
benchB' name x b = bench name $ whnf (S.length . b) x


hashInt :: Int -> Int
hashInt x = iterate step x !! 10
  where
    step a = e
      where b = (a `xor` 61) `xor` (a `shiftR` 16)
            c = b + (b `shiftL` 3)
            d = c `xor` (c `shiftR` 4)
            e = d * 0x27d4eb2d

w :: Int -> Word8
w = fromIntegral

hashWord8 :: Word8 -> Word8
hashWord8 = fromIntegral . hashInt . fromIntegral

partitionStrict :: (Word8 -> Bool) -> Benchmarkable
partitionStrict p = nf (S.partition p) . randomStrict $ mkStdGen 98423098
  where randomStrict = fst . S.unfoldrN 10000 (Just . random)


-- benchmarks
-------------


foldInputs :: [ShortByteString]
foldInputs = map (\k -> S.pack $ if (k :: Int) <= 6 then take (2 ^ k) [32..95] else concat (replicate (2 ^ (k - 6)) [32..95])) [0..16]

largeTraversalInput :: ShortByteString
largeTraversalInput = S.concat (replicate 10 byteStringData)

smallTraversalInput :: ShortByteString
smallTraversalInput = S.toShort . S8.unlines $ map S8.pack ["The quick brown fox"]

benchmarks :: [ Benchmark ]
benchmarks =
    [ bgroup "AFP.Data.ByteString.Short"
      [ bgroup "Small payload"
        [ benchB' "mempty"        ()  (const mempty)
        , benchB' "UTF-8 String (naive)" "hello world\0" fromString
        , benchB' "String (naive)" "hello world!" fromString
        ]
      ]

    , bgroup "partition"
      [
        bgroup "strict"
        [
          bench "mostlyTrueFast"  $ partitionStrict (< (w 225))
        , bench "mostlyFalseFast" $ partitionStrict (< (w 10))
        , bench "balancedFast"    $ partitionStrict (< (w 128))

        , bench "mostlyTrueSlow"  $ partitionStrict (\x -> hashWord8 x < w 225)
        , bench "mostlyFalseSlow" $ partitionStrict (\x -> hashWord8 x < w 10)
        , bench "balancedSlow"    $ partitionStrict (\x -> hashWord8 x < w 128)
        ]
      ]
    , bgroup "folds"
      [ bgroup "foldl'" $ map (\s -> bench (show $ S.length s) $
          nf (S.foldl' (\acc x -> acc + fromIntegral x) (0 :: Int)) s) foldInputs
      , bgroup "foldr'" $ map (\s -> bench (show $ S.length s) $
          nf (S.foldr' (\x acc -> fromIntegral x + acc) (0 :: Int)) s) foldInputs
      , bgroup "filter" $ map (\s -> bench (show $ S.length s) $
          nf (S.filter odd) s) foldInputs
      ]
    , bgroup "findIndex_"
      [ bench "find"           $ nf (S.find (>= 198)) byteStringData
      ]
    , bgroup "traversals"
      [ bench "map (+1) large" $ nf (S.map (+ 1)) largeTraversalInput
      , bench "map (+1) small" $ nf (S.map (+ 1)) smallTraversalInput
      ]
    , benchIndices
    ]
