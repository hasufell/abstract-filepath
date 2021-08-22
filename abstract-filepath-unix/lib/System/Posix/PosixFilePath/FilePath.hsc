{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.PosixFilePath.FilePath
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- Internal stuff: support for ByteString FilePaths
--
-----------------------------------------------------------------------------

module System.Posix.PosixFilePath.FilePath (
     withFilePath, peekFilePath, peekFilePathLen,
     throwErrnoPathIfMinus1Retry,
     throwErrnoPathIfMinus1Retry_,
     throwErrnoPathIfNullRetry,
     throwErrnoPathIfRetry,
     throwErrnoPath,
     throwErrnoPathIf,
     throwErrnoPathIf_,
     throwErrnoPathIfNull,
     throwErrnoPathIfMinus1,
     throwErrnoPathIfMinus1_,
     throwErrnoTwoPathsIfMinus1_,
     _toStr,
     _toStr'
  ) where

import Foreign hiding ( void )
import Foreign.C hiding (
     throwErrnoPath,
     throwErrnoPathIf,
     throwErrnoPathIf_,
     throwErrnoPathIfNull,
     throwErrnoPathIfMinus1,
     throwErrnoPathIfMinus1_ )

import AFP.AbstractFilePath.Types
import AFP.OsString.Internal.Types
import Control.Monad
import "shortbytestring" Data.ByteString.Short
import Prelude hiding (FilePath)
#if !MIN_VERSION_base(4, 11, 0)
import Data.Monoid ((<>))
#endif


withFilePath :: PosixFilePath -> (CString -> IO a) -> IO a
withFilePath = useAsCString . unPFP

peekFilePath :: CString -> IO PosixFilePath
peekFilePath = fmap PS . packCString

peekFilePathLen :: CStringLen -> IO PosixFilePath
peekFilePathLen = fmap PS . packCStringLen


throwErrnoPathIfMinus1Retry :: (Eq a, Num a)
                            => String -> PosixFilePath -> IO a -> IO a
throwErrnoPathIfMinus1Retry loc path f = do
  throwErrnoPathIfRetry (== -1) loc path f

throwErrnoPathIfMinus1Retry_ :: (Eq a, Num a)
                             => String -> PosixFilePath -> IO a -> IO ()
throwErrnoPathIfMinus1Retry_ loc path f =
  void $ throwErrnoPathIfRetry (== -1) loc path f

throwErrnoPathIfNullRetry :: String -> PosixFilePath -> IO (Ptr a) -> IO (Ptr a)
throwErrnoPathIfNullRetry loc path f =
  throwErrnoPathIfRetry (== nullPtr) loc path f

throwErrnoPathIfRetry :: (a -> Bool) -> String -> PosixFilePath -> IO a -> IO a
throwErrnoPathIfRetry pr loc rpath f =
  do
    res <- f
    if pr res
      then do
        err <- getErrno
        if err == eINTR
          then throwErrnoPathIfRetry pr loc rpath f
          else throwErrnoPath loc rpath
      else return res

-- | as 'throwErrno', but exceptions include the given path when appropriate.
--
throwErrnoPath :: String -> PosixFilePath -> IO a
throwErrnoPath loc path =
  do
    errno <- getErrno
    ioError (errnoToIOError loc errno Nothing (Just (_toStr path)))

-- | as 'throwErrnoIf', but exceptions include the given path when
--   appropriate.
--
throwErrnoPathIf :: (a -> Bool) -> String -> PosixFilePath -> IO a -> IO a
throwErrnoPathIf cond loc path f =
  do
    res <- f
    if cond res then throwErrnoPath loc path else return res

-- | as 'throwErrnoIf_', but exceptions include the given path when
--   appropriate.
--
throwErrnoPathIf_ :: (a -> Bool) -> String -> PosixFilePath -> IO a -> IO ()
throwErrnoPathIf_ cond loc path f  = void $ throwErrnoPathIf cond loc path f

-- | as 'throwErrnoIfNull', but exceptions include the given path when
--   appropriate.
--
throwErrnoPathIfNull :: String -> PosixFilePath -> IO (Ptr a) -> IO (Ptr a)
throwErrnoPathIfNull  = throwErrnoPathIf (== nullPtr)

-- | as 'throwErrnoIfMinus1', but exceptions include the given path when
--   appropriate.
--
throwErrnoPathIfMinus1 :: (Eq a, Num a) => String -> PosixFilePath -> IO a -> IO a
throwErrnoPathIfMinus1 = throwErrnoPathIf (== -1)

-- | as 'throwErrnoIfMinus1_', but exceptions include the given path when
--   appropriate.
--
throwErrnoPathIfMinus1_ :: (Eq a, Num a) => String -> PosixFilePath -> IO a -> IO ()
throwErrnoPathIfMinus1_  = throwErrnoPathIf_ (== -1)

-- | as 'throwErrnoTwoPathsIfMinus1_', but exceptions include two paths when appropriate.
--
throwErrnoTwoPathsIfMinus1_ :: (Eq a, Num a) => String -> PosixFilePath -> PosixFilePath -> IO a -> IO ()
throwErrnoTwoPathsIfMinus1_  loc path1 path2 =
    throwErrnoIfMinus1_ (loc <> " '" <> _toStr path1 <> "' to '" <> _toStr path2 <> "'")


_toStr :: PosixFilePath -> String
_toStr (PS fp) = fmap (toEnum . fromIntegral) $ unpack fp

_toStr' :: ShortByteString -> String
_toStr' fp = fmap (toEnum . fromIntegral) $ unpack fp
