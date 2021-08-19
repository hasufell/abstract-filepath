{-# LANGUAGE CPP #-}
module AFP.AbstractFilePath.Types where

import AFP.OsString.Internal.Types


-- | Filepaths are UTF16 data on windows as passed to syscalls.
type WindowsFilePath = WindowsString

-- | Filepaths are @char[]@ data on unix as passed to syscalls.
type PosixFilePath = PosixString

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
type PlatformFilePath = WindowsFilePath
#else
type PlatformFilePath = PosixFilePath
#endif


-- | Type representing filenames\/pathnames.
--
-- This type doesn't add any guarantees over 'OsString'.
type AbstractFilePath = OsString
