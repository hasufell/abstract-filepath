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
module OsString
  (
  -- * Types
    OsString
  , WindowsString
  , PosixString

  -- * Construction
  , toOsString
  , toOsString'
  , fromByteString
  , osstr

  -- * Deconstruction
  , fromOsString
  , fromOsString'
  )
where

import OsString.Internal
    ( fromByteString
    , fromOsString
    , fromOsString'
    , osstr
    , toOsString
    , toOsString'
    )
import OsString.Internal.Types
    ( OsString, PosixString, WindowsString )
