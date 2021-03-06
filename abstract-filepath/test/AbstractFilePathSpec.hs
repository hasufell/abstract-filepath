{-# LANGUAGE ScopedTypeVariables #-}

module AbstractFilePathSpec where

import AFP.AbstractFilePath
import AFP.OsString.Internal.Types
import Data.ByteString.Short.Decode
    ( decodeUtf16LE, decodeUtf8 )
import Data.ByteString.Short.Encode
    ( encodeUtf16LE, encodeUtf8 )

import Arbitrary ()
import Test.Hspec
    ( Spec, describe, it )
import Test.Hspec.Checkers
import Test.QuickCheck
    ( Testable (property) )
import Test.QuickCheck.Classes


spec :: Spec
spec = do
  describe "Encoding/Decoding" $ do
    it "decodeUtf8 . encodeUtf8 == id" $
      property $ \str -> (decodeUtf8 . encodeUtf8) str == str

    it "decodeUtf16LE . encodeUtf16LE == id" $
      property $ \str -> (decodeUtf16LE . encodeUtf16LE) str == str

    it "fromAbstractFilePath . toAbstractFilePath == id" $
      property $ \str -> (fromAbstractFilePath . toAbstractFilePath) str == Just str

  testBatch (ord (\(a :: AbstractFilePath) -> pure a))
  testBatch (monoid (undefined :: AbstractFilePath))

  testBatch (ord (\(a :: OsString) -> pure a))
  testBatch (monoid (undefined :: OsString))

  testBatch (ord (\(a :: WindowsString) -> pure a))
  testBatch (monoid (undefined :: WindowsString))

  testBatch (ord (\(a :: PosixString) -> pure a))
  testBatch (monoid (undefined :: PosixString))

  testBatch (ord (\(a :: PlatformString) -> pure a))
  testBatch (monoid (undefined :: PlatformString))
