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

module Short.Word16.BenchAll (benchmarks) where

import Short.Word16.BenchIndices

import           Data.Foldable                         (foldMap)
import           Data.Monoid
import           Data.String
import           Test.Tasty.Bench
import           Prelude                               hiding (words)

import qualified Data.ByteString.Char8                 as S8
import           Data.ByteString.Short                 ( ShortByteString )
import qualified Data.ByteString.Short                 as Short
import qualified AFP.Data.ByteString.Short.Word16      as S


import           Foreign

import System.Random


------------------------------------------------------------------------------
-- Benchmark support
------------------------------------------------------------------------------

countToZero :: Int -> Maybe (Int, Int)
countToZero 0 = Nothing
countToZero n = Just (n, n - 1)


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

{-# NOINLINE smallIntegerData #-}
smallIntegerData :: [Integer]
smallIntegerData = map fromIntegral intData

{-# NOINLINE largeIntegerData #-}
largeIntegerData :: [Integer]
largeIntegerData = map (* (10 ^ (100 :: Integer))) smallIntegerData


{-# NOINLINE floatData #-}
floatData :: [Float]
floatData = map (\x -> (3.14159 * fromIntegral x) ^ (3 :: Int)) intData

{-# NOINLINE doubleData #-}
doubleData :: [Double]
doubleData = map (\x -> (3.14159 * fromIntegral x) ^ (3 :: Int)) intData

{-# NOINLINE byteStringData #-}
byteStringData :: ShortByteString
byteStringData = S.pack $ map fromIntegral intData

{-# NOINLINE byteStringChunksData #-}
byteStringChunksData :: [ShortByteString]
byteStringChunksData = map (S.pack . replicate (4 ) . fromIntegral) intData

{-# NOINLINE loremIpsum #-}
loremIpsum :: ShortByteString
loremIpsum = S.toShort . S8.unlines $ map S8.pack
  [ "  Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor"
  , "incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis"
  , "nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat."
  , "Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu"
  , "fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in"
  , "culpa qui officia deserunt mollit anim id est laborum."
  ]

-- benchmark wrappers
---------------------

{-# INLINE benchB #-}
benchB :: String -> a -> (a -> ShortByteString) -> Benchmark
benchB name x b =
    bench (name ++" (" ++ show nRepl ++ ")") $
        whnf (S.length . b) x

{-# INLINE benchB' #-}
benchB' :: String -> a -> (a -> ShortByteString) -> Benchmark
benchB' name x b = bench name $ whnf (S.length . b) x

{-# INLINE benchBInts #-}
benchBInts :: String -> ([Int] -> ShortByteString) -> Benchmark
benchBInts name = benchB name intData


hashInt :: Int -> Int
hashInt x = iterate step x !! 10
  where
    step a = e
      where b = (a `xor` 61) `xor` (a `shiftR` 16)
            c = b + (b `shiftL` 3)
            d = c `xor` (c `shiftR` 4)
            e = d * 0x27d4eb2d
            f = e `xor` (e `shiftR` 15)

w :: Int -> Word16
w = fromIntegral

hashWord16 :: Word16 -> Word16
hashWord16 = fromIntegral . hashInt . fromIntegral

partitionStrict p = nf (S.partition p) . randomStrict $ mkStdGen 98423098
  where randomStrict = fst . S.unfoldrN 10000 (Just . random)

easySubstrings, randomSubstrings :: Int -> Int -> (ShortByteString, ShortByteString)
hardSubstrings, pathologicalSubstrings :: Int ->
                                          Int -> (ShortByteString, ShortByteString)

{-# INLINE easySubstrings #-}
easySubstrings n h = (S.replicate n $ w 1,
                      S.replicate h $ w 0)

{-# INLINE randomSubstrings #-}
randomSubstrings n h = (f 48278379 n, f 98403980 h)
  where
    next' g = let (x, g') = next g in (w x, g')
    f g l = fst $ S.unfoldrN l (Just . next') (mkStdGen g)

{-# INLINE hardSubstrings #-}
hardSubstrings n h = (f 48278379 n, f 98403980 h)
  where
    next' g = let (x, g') = next g
              in (w $ x `mod` 4, g')
    f g l = fst $ S.unfoldrN l (Just . next') (mkStdGen g)

{-# INLINE pathologicalSubstrings #-}
pathologicalSubstrings n h =
  (S.replicate n (w 0),
   S.concat . replicate (h `div` n) $ S.replicate (n - 1) (w 0) `S.snoc` w 1)

htmlSubstrings :: ShortByteString -> Int -> Int -> IO (ShortByteString, ShortByteString)
htmlSubstrings s n h =
    do i <- randomRIO (0, l - n)
       return (S.take n . S.drop i $ s', s')
  where
    s' = S.take h s
    l  = S.length s'

-- benchmarks
-------------

sortInputs :: [ShortByteString]
sortInputs = map (`S.take` S.pack [122, 121 .. 32]) [10..25]

foldInputs :: [ShortByteString]
foldInputs = map (\k -> S.pack $ if k <= 6 then take (2 ^ k) [32..95] else concat (replicate (2 ^ (k - 6)) [32..95])) [0..16]

largeTraversalInput :: ShortByteString
largeTraversalInput = S.concat (replicate 10 byteStringData)

smallTraversalInput :: ShortByteString
smallTraversalInput = S.toShort . S8.unlines $ map S8.pack ["The quick brown fox"]

benchmarks :: [ Benchmark ]
benchmarks =
    [ bgroup "AFP.Data.ByteString.Short.Word16"
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

        , bench "mostlyTrueSlow"  $ partitionStrict (\x -> hashWord16 x < w 225)
        , bench "mostlyFalseSlow" $ partitionStrict (\x -> hashWord16 x < w 10)
        , bench "balancedSlow"    $ partitionStrict (\x -> hashWord16 x < w 128)
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
