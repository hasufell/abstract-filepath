{-# LANGUAGE ScopedTypeVariables #-}

module AbstractFilePathSpec where

import AbstractFilePath
import AbstractFilePath.Internal.Decode
    ( decodeUtf16LE, decodeUtf8 )
import AbstractFilePath.Internal.Encode
    ( encodeUtf16LE, encodeUtf8 )

import Arbitrary
import Test.Hspec ( describe, it, Spec )
import Test.Hspec.Checkers
import Test.QuickCheck ( Testable(property) )
import Test.QuickCheck.Classes


spec :: Spec
spec = do
  describe "Encoding/Decoding" $ do
    it "decodeUtf8 . encodeUtf8 == id" $
      property $ \str -> (decodeUtf8 . encodeUtf8) str == str

    it "decodeUtf16LE . encodeUtf16LE == id" $
      property $ \str -> (decodeUtf16LE . encodeUtf16LE) str == str

    it "fromAbstractFilePath . toAbstractFilePath == id" $
      property $ \str -> (fromAbstractFilePath . toAbstractFilePath) str == str

  testBatch (ord (\(a :: AbstractFilePath) -> pure a))
  testBatch (monoid (undefined :: AbstractFilePath))
