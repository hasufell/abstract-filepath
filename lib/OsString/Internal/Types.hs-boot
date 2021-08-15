{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
module OsString.Internal.Types where

import qualified "bytestring" Data.ByteString.Short as BS

newtype WindowsString = WS { unWFP :: BS.ShortByteString }
newtype PosixString   = PS { unPFP :: BS.ShortByteString }

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
type PlatformString = WindowsString
#else
type PlatformString = PosixString
#endif

newtype OsString = OsString PlatformString

