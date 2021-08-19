{-# LANGUAGE CPP #-}
#define FILEPATH_NAME   PosixFilePath
#define OSSTRING_NAME   PosixString
#define WORD_NAME       PosixChar
#define CTOR            PS
#define WTOR            PW
#define IS_WINDOWS      False
#define POSIX
#undef WINDOWS


module AFP.AbstractFilePath.Posix
  (
  -- * Types
    PosixString
  , PosixChar
  , PosixFilePath
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

  -- * posix specific functions
  , hiddenFile
  , isSpecialDirectoryEntry
  )
where

import AFP.AbstractFilePath.Types
import AFP.OsString.Internal.Types

import AFP.OsString.Posix
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

import qualified AFP.AbstractFilePath.Internal.Posix as C

import AFP.AbstractFilePath.Types
  ()


-- $setup
-- >>> :set -XFlexibleInstances
-- >>> import Data.Char
-- >>> import Data.Maybe
-- >>> import Data.Word8
-- >>> import Test.QuickCheck
-- >>> import Control.Applicative
-- >>> import AFP.AbstractFilePath.Types
-- >>> import AFP.OsString.Internal.Types
-- >>> import qualified Data.ByteString.Short as BS
-- >>> import Data.ByteString.Short (ShortByteString)
-- >>> instance Arbitrary ShortByteString where arbitrary = BS.pack <$> arbitrary
-- >>> instance CoArbitrary ShortByteString where coarbitrary = coarbitrary . BS.unpack
-- >>> instance Arbitrary PosixFilePath where arbitrary = PS <$> arbitrary
-- >>> instance CoArbitrary PosixFilePath where coarbitrary = coarbitrary . (\(PS fp) -> fp)
-- >>> instance Arbitrary ShortByteString where arbitrary = sized $ \n -> choose (0,n) >>= \k -> fmap BS.pack $ vectorOf (if even k then k else k + 1) arbitrary
-- >>> instance Arbitrary PosixString where arbitrary = PS <$> arbitrary
-- >>> let _chr :: Word -> Char; _chr = chr . fromIntegral



#include "Common.hs"
