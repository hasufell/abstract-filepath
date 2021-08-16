-- |
-- Module      :  OsString
-- Copyright   :  © 2021 Julian Ospald
-- License     :  MIT
--
-- Maintainer  :  Julian Ospald <hasufell@posteo.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- An implementation of platform specific 'OsString', which is:
--
-- 1. on windows UTF16 data
-- 2. on unix UTF8 data
--
-- It captures the notion of syscall specific encoding to avoid roundtrip issues
-- and memory fragmentation by using unpinned byte arrays.
module AFP.OsString
  (
  -- * String types
    OsString
  , WindowsString
  , PosixString

  -- * String construction
  , toOsString
  , toOsStringIO
  , bsToOsString
  , osstr
  , packOsString

  -- * String deconstruction
  , fromOsString
  , fromOsStringIO
  , unpackOsString

  -- * Word types
  , OsWord
  , WindowsWord
  , PosixWord

  -- * Word construction
  , fromChar
  )
where

import AFP.OsString.Internal
    ( bsToOsString
    , fromChar
    , fromOsString
    , fromOsStringIO
    , osstr
    , packOsString
    , toOsString
    , toOsStringIO
    , unpackOsString
    )
import AFP.OsString.Internal.Types
    ( OsString, OsWord, PosixString, PosixWord, WindowsString, WindowsWord )
