{-# LANGUAGE CPP #-}

-- |
-- Module      :  AbstractFilePath
-- Copyright   :  © 2021 Julian Ospald
-- License     :  MIT
--
-- Maintainer  :  Julian Ospald <hasufell@posteo.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- An implementation of the <https://gitlab.haskell.org/ghc/ghc/-/wikis/proposal/abstract-file-path Abstract FilePath Proposal>,
-- which aims to supersede @type FilePath = String@ for various reasons:
--
-- 1. it is more efficient (uses unpinned 'ShortByteString' under the hood)
-- 2. is more type-safe (not a type synonym, but a newtype)
-- 3. avoids round-tripping issues, by not converting to String (which loses the encoding)
--
-- It is important to know that filenames\/filepaths have different representations across platforms:
--
-- - On /Windows/, filepaths are expected to be in UTF16 as passed to
--   syscalls (although there are other APIs, the <https://hackage.haskell.org/package/Win32 Win32> package uses the wide character one).
--   This invariant is maintained by 'AbstractFilePath'.
-- - On /Unix/, filepaths don't have a predefined encoding (although they
--   are often interpreted as UTF8) as per the
--   <https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap03.html#tag_03_170 POSIX specification>
--   and are passed as @char[]@ to syscalls. 'AbstractFilePath' maintains no invariant
--   here. Some functions however, such as 'toAbstractFilePath', may expect
--   or produce UTF8.
--
-- Apart from encoding, filepaths have additional restrictions per platform:
--
-- - On /Windows/ the <https://docs.microsoft.com/en-us/windows/win32/fileio/naming-a-file#naming-conventions naming convention> may apply
-- - On /Unix/, only @NUL@ bytes are disallowed as per the <https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap03.html#tag_03_170 POSIX specification>
--
-- Use 'filePathIsValid' to check for these restrictions ('AbstractFilePath' doesn't
-- maintain this invariant).
--
-- Also note that these restrictions are
-- not exhaustive and further filesystem specific restrictions may apply on
-- all platforms. This library makes no attempt at satisfying these.
-- Library users may need to account for that, depending
-- on what filesystems they want to support.
--
-- It is advised to follow these principles when dealing with filepaths\/filenames:
--
-- 1. Avoid interpreting filenames that the OS returns, unless absolutely necessary.
--    For example, the filepath separator is usually a predefined 'Word8', regardless of encoding.
--    So even if we need to split filepaths, it might still not be necessary to understand the encoding
--    of the filename.
-- 2. When interpreting OS returned filenames consider that these might not be UTF8 on /unix/
--    or at worst don't have an ASCII compatible encoding. Some strategies here involve looking
--    up the current locale and using that for decoding ('fromAbstractFilePathIO' does this).
--    Otherwise it can be reasonable to assume UTF8 on unix ('fromAbstractFilePath' does that) if your application specifically
--    mentions that it requires a UTF8 compatible system. These things should be documented.
-- 3. When dealing with user input (e.g. on the command line) on /unix/ as e.g. @String@ the input
--    encoding is lost. The output encoding (e.g. how we write a filename to disk) can then
--    either follow the current locale again ('toAbstractFilePath'') or a fixed encoding
--    ('toAbstractFilePath'). The decision should be clearly documented. If the input is in the
--    form of a @ByteString@, then 'bsToAFP' may be of interest, unless the input needs further
--    interpretation.

module AFP.AbstractFilePath
  (
  -- * Types
    AbstractFilePath
  , OsString
  , OsChar

  -- * Construction
  , toAbstractFilePath
  , toAbstractFilePathIO
  , bsToAFP
  , afp
  , packAFP

  -- * Deconstruction
  , fromAbstractFilePath
  , fromAbstractFilePathIO
  , unpackAFP

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

import AFP.AbstractFilePath.Internal
    ( afp
    , bsToAFP
    , fromAbstractFilePath
    , fromAbstractFilePathIO
    , packAFP
    , toAbstractFilePath
    , toAbstractFilePathIO
    , unpackAFP
    )
import AFP.AbstractFilePath.Types
    ( AbstractFilePath )
import AFP.OsString
import AFP.OsString.Internal.Types

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import qualified AFP.AbstractFilePath.Windows as C
import qualified AFP.Data.ByteString.Short.Word16 as BS
import AFP.Data.Word16
    ( Word16, _period )
#else
import qualified AFP.AbstractFilePath.Posix as C
#endif

import Data.Bifunctor
    ( bimap )


#define FILEPATH_NAME   AbstractFilePath
#define OSSTRING_NAME   OsString
#define WORD_NAME       OsChar
#define CTOR            OsString
#define WTOR            OsChar

#include "AbstractFilePath/Common.hs"
