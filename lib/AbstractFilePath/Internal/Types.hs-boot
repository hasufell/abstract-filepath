{-# LANGUAGE CPP #-}
module AbstractFilePath.Internal.Types where

import qualified Data.ByteString.Short as BS

newtype WindowsString = WFP { unWFP :: BS.ShortByteString }
newtype PosixString   = PFP { unPFP :: BS.ShortByteString }

type WindowsFilePath = WindowsString
type PosixFilePath = PosixString

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
type PlatformFilePath = WindowsFilePath
#else
type PlatformFilePath = PosixFilePath
#endif

newtype AbstractFilePath = AbstractFilePath PlatformFilePath
