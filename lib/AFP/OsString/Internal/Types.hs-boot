{-# LANGUAGE CPP #-}
module AFP.OsString.Internal.Types where

import AFP.Data.Word16
import Data.Word8
import qualified Data.ByteString.Short as BS

newtype WindowsString = WS { unWFP :: BS.ShortByteString }
newtype PosixString   = PS { unPFP :: BS.ShortByteString }

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
type PlatformString = WindowsString
#else
type PlatformString = PosixString
#endif

newtype WindowsWord = WW Word16
newtype PosixWord   = PW Word8

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
type PlatformWord = WindowsWord
#else
type PlatformWord = PosixWord
#endif

newtype OsString = OsString PlatformString

newtype OsWord = OsWord PlatformWord
