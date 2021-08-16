{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module AFP.AbstractFilePath.Internal.Types where

import AFP.OsString.Internal.Types

import Data.ByteString
    ( ByteString )
import Data.Proxy
    ( Proxy (..) )
import Data.Typeable
import GHC.Exts
    ( IsString (..) )
import GHC.IO.Encoding
    ( getFileSystemEncoding )
import GHC.IO.Exception
    ( IOErrorType (InvalidArgument) )
import Language.Haskell.TH
import Language.Haskell.TH.Quote
    ( QuasiQuoter (..) )
import Language.Haskell.TH.Syntax
    ( Lift (..), lift )
import System.IO.Error
    ( catchIOError )

import qualified Data.ByteString.Short as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified GHC.Foreign as GHC
import qualified Language.Haskell.TH.Syntax as TH


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
