{-# LANGUAGE CPP #-}
#define FILEPATH_NAME   WindowsFilePath
#define OSSTRING_NAME   WindowsString
#define WORD_NAME       WindowsWord
#define CTOR            WS
#define WTOR            WW
#define IS_WINDOWS      True
#define WINDOWS

module AFP.AbstractFilePath.Windows
  (
  -- * Types
    WindowsString
  , WindowsWord
  , WindowsFilePath
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


import AFP.AbstractFilePath.Types
import AFP.OsString.Internal.Types

import AFP.OsString.Windows
    ( bsToPlatformString
    , fromChar
    , fromPlatformString
    , fromPlatformStringIO
    , packPlatformString
    , pstr
    , toPlatformString
    , toPlatformStringIO
    , unpackPlatformString
    )

import Data.Bifunctor
    ( bimap )

import qualified AFP.AbstractFilePath.Internal.Windows as C

import AFP.AbstractFilePath.Types
  ()


-- $setup
-- >>> :set -XFlexibleInstances
-- >>> import Data.Char
-- >>> import Data.Maybe
-- >>> import AFP.Data.Word16
-- >>> import Test.QuickCheck
-- >>> import Control.Applicative
-- >>> import AFP.AbstractFilePath.Types
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
