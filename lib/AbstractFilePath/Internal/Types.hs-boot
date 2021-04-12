{-# LANGUAGE CPP #-}
module AbstractFilePath.Internal.Types where

import qualified Data.ByteString.Short as BS

newtype WindowsFilePath = WFP BS.ShortByteString 
newtype PosixFilePath   = PFP BS.ShortByteString

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
type PlatformFilePath = WindowsFilePath
#else
type PlatformFilePath = PosixFilePath
#endif

newtype AbstractFilePath = AbstractFilePath PlatformFilePath
