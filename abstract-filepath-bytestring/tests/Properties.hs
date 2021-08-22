{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}


import Test.Tasty

import qualified Properties.ByteString.Short as PropShort
import qualified Properties.ByteString.Short.Word16 as PropShortWord16


------------------------------------------------------------------------
-- The entry point

main :: IO ()
main = defaultMain $ testGroup "All"
  [ testGroup "StrictWord8" PropShort.tests
  , testGroup "StrictWord16" PropShortWord16.tests
  ]

