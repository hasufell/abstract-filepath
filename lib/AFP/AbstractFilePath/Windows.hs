{-# LANGUAGE CPP #-}
#define MODULE_NAME     Windows
#define FILEPATH_NAME   WindowsFilePath
#define OSSTRING_NAME   WindowsString
#define WORD_NAME       WindowsWord
#define CTOR            WS
#define WTOR            WW
#define IS_WINDOWS      True
#define WINDOWS

module AFP.AbstractFilePath.MODULE_NAME
  (
  -- * Types
#ifdef WINDOWS
    WindowsString
  , WindowsWord
  , WindowsFilePath
#else
    PosixString
  , PosixWord
  , PosixFilePath
#endif
  -- * String construction
  , toPlatformString
  , toPlatformStringIO
  , bsToPlatformString
  , pstr
  , packPlatformString

  -- * String deconstruction
  , fromPlatformString
  , fromPlatformStringIO
  , unpackPlatformString

  -- * Word construction
  , fromChar

  -- * Separator predicates
  , pathSeparator
  , pathSeparators
  , isPathSeparator
  , searchPathSeparator
  , isSearchPathSeparator
  , extSeparator
  , isExtSeparator

  -- * $PATH methods
  , splitSearchPath

  -- * Extension functions
  , takeExtension
  , replaceExtension
  , dropExtension
  , addExtension
  , hasExtension
  , (<.>)
  , splitExtensions
  , dropExtensions
  , takeExtensions
  , splitExtension
  , stripExtension

  -- * Filename\/directory functions
  , splitFileName
  , takeFileName
  , replaceFileName
  , dropFileName
  , takeBaseName
  , replaceBaseName
  , takeDirectory
  , replaceDirectory
  , combine
  , (</>)
  , splitPath
  , joinPath
  , splitDirectories
  , takeAllParents

  -- * Drive functions
  , splitDrive
  , joinDrive
  , takeDrive
  , hasDrive
  , dropDrive
  , isDrive

  -- * Trailing slash functions
  , hasTrailingPathSeparator
  , addTrailingPathSeparator
  , dropTrailingPathSeparator

  -- * File name manipulations
  , normalise
  , makeRelative
  , equalFilePath
  , isRelative
  , isAbsolute
  , isValid
  , makeValid
  , isFileName
  , hasParentDir
  )
where

-- doctest
import AFP.AbstractFilePath.Internal.Types
    ()

import AFP.AbstractFilePath.Internal.Types
import AFP.OsString.Internal.Types

import AFP.OsString.MODULE_NAME (
    toPlatformString
  , toPlatformStringIO
  , bsToPlatformString
  , pstr
  , packPlatformString
  , fromPlatformString
  , fromPlatformStringIO
  , unpackPlatformString
  , fromChar
  )


import Control.Arrow
    ( second )
import Data.Bifunctor
    ( bimap, first )
import Data.ByteString
    ( ByteString )
import Data.ByteString.Short
    ( ShortByteString )
import Data.Maybe
    ( isJust )
import Data.Word8
    ( Word8, _colon, _nul, _period, _slash, _underscore )

import qualified AFP.AbstractFilePath.Internal.Windows as C
import qualified AFP.Data.ByteString.Short.Word16 as BS


-- $setup
-- >>> :set -XFlexibleInstances
-- >>> import Data.Char
-- >>> import Data.Maybe
-- >>> import Data.Word8
-- >>> import Test.QuickCheck
-- >>> import Control.Applicative
-- >>> import AFP.AbstractFilePath.Internal.Types
-- >>> import AFP.OsString.Internal.Types
-- >>> import Data.ByteString.Short (ShortByteString)
-- >>> import AFP.Data.ByteString.Short as BS (concat)
-- >>> import qualified AFP.Data.ByteString.Short.Word16 as BS
-- >>> instance Arbitrary ShortByteString where arbitrary = BS.pack <$> arbitrary
-- >>> instance CoArbitrary ShortByteString where coarbitrary = coarbitrary . BS.unpack
-- >>> instance Arbitrary WindowsFilePath where arbitrary = WS <$> arbitrary
-- >>> instance CoArbitrary WindowsFilePath where coarbitrary = coarbitrary . (\(WS fp) -> fp)
-- >>> instance Arbitrary ShortByteString where arbitrary = sized $ \n -> choose (0,n) >>= \k -> fmap BS.pack $ vectorOf (if even k then k else k + 1) arbitrary
-- >>> instance Arbitrary WindowsString where arbitrary = WS <$> arbitrary
-- >>> let _chr :: Word -> Char; _chr = chr . fromIntegral


#include "Common.hs"
