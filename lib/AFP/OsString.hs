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
module AFP.OsString
  (
  -- * String types
    OsString
  , WindowsString
  , PosixString

  -- * String construction
  , toOsString
  , toOsString'
  , fromByteString
  , osstr
  , packOsString

  -- * String deconstruction
  , fromOsString
  , fromOsString'
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
    ( fromByteString
    , fromChar
    , fromOsString
    , fromOsString'
    , osstr
    , packOsString
    , toOsString
    , toOsString'
    , unpackOsString
    )
import AFP.OsString.Internal.Types
    ( OsString, OsWord, PosixString, PosixWord, WindowsString, WindowsWord )
