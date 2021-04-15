{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module AbstractFilePath.Internal.Types where

import {-# SOURCE #-} AbstractFilePath.Internal (toAbstractFilePath)

import AbstractFilePath.Internal.Encode (encodeUtf16LE, encodeUtf8)
import AbstractFilePath.Internal.Decode
    ( decodeUtf16LE, decodeUtf8 )

import Data.ByteString ( ByteString )
import Data.Proxy ( Proxy (..) )
import Data.Typeable
import GHC.Exts ( IsString(..) )
import GHC.IO.Encoding ( getFileSystemEncoding )
import GHC.IO.Exception (IOErrorType(InvalidArgument) )
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift(..), lift)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import System.IO.Error (catchIOError)

import qualified Data.ByteString.Short as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified GHC.Foreign as GHC
import qualified Language.Haskell.TH.Syntax as TH

-- Using unpinned bytearrays to avoid Heap fragmentation and
-- which are reasonably cheap to pass to FFI calls
-- wrapped with typeclass-friendly types allowing to avoid CPP
-- 
-- Note that, while unpinned bytearrays incur a memcpy on each
-- FFI call, this overhead is generally much preferable to
-- the memory fragmentation of pinned bytearrays

-- | Commonly used windows string as UTF16 bytes.
newtype WindowsString = WFP { unWFP :: BS.ShortByteString }
  deriving (Eq, Ord, Semigroup, Monoid)
-- | Commonly used Posix string as uninterpreted @char[]@
-- array.
newtype PosixString   = PFP { unPFP :: BS.ShortByteString }
  deriving (Eq, Ord, Semigroup, Monoid)

-- | Filepaths are UTF16 data on windows as passed to syscalls.
type WindowsFilePath = WindowsString

-- | Filepaths are @char[]@ data on unix as passed to syscalls.
type PosixFilePath = PosixString

instance Show WindowsString where
  show (WFP bs) = ('\"': decodeUtf16LE bs) <> "\""

instance Show PosixString where
  show (PFP bs) = ('\"': decodeUtf8 bs) <> "\""

instance IsString WindowsString where 
    fromString = WFP . encodeUtf16LE

instance IsString PosixString where 
    fromString = PFP . encodeUtf8

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
type PlatformFilePath = WindowsFilePath
type PlatformString = WindowsString
#else
type PlatformFilePath = PosixFilePath
type PlatformString = PosixString
#endif


-- | Type representing filenames\/pathnames.
--
-- Internally this is either 'WindowsFilePath' or 'PosixFilePath',
-- depending on the platform. Both use unpinned
-- 'ShortByteString' for efficiency and correctness.
--
-- The constructor is only exported via "AbstractFilePath.Internal.Types", since
-- dealing with the internals isn't generally recommended, but supported
-- in case you need to write platform specific code, such as the implementation
-- of 'fromAbstractFilePath'.
newtype AbstractFilePath = AbstractFilePath PlatformFilePath
  deriving Show

-- | Byte equality of the internal representation.
instance Eq AbstractFilePath where
  (AbstractFilePath a) == (AbstractFilePath b) = a == b

-- | Byte ordering of the internal representation.
instance Ord AbstractFilePath where
  compare (AbstractFilePath a) (AbstractFilePath b) = compare a b

-- | Encodes as UTF16 on windows and UTF8 on unix.
instance IsString AbstractFilePath where 
    fromString = toAbstractFilePath

-- | \"String-Concatenation\" for 'AbstractFilePath'. This is __not__ the same
-- as '(</>)'.
instance Monoid AbstractFilePath where 
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
    mempty      = AbstractFilePath (WFP BS.empty)
    mappend (AbstractFilePath (WFP a)) (AbstractFilePath (WFP b))
      = AbstractFilePath (WFP (mappend a b))
#else
    mempty      = AbstractFilePath (PFP BS.empty)
    mappend (AbstractFilePath (PFP a)) (AbstractFilePath (PFP b))
      = AbstractFilePath (PFP (mappend a b))
#endif
#if MIN_VERSION_base(4,11,0)
instance Semigroup AbstractFilePath where 
    (<>) = mappend
#endif


instance Lift AbstractFilePath where
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
  lift (AbstractFilePath (WFP bs))
    = [| AbstractFilePath (WFP (BS.pack $(lift $ BS.unpack bs))) :: AbstractFilePath |]
#else
  lift (AbstractFilePath (PFP bs))
    = [| AbstractFilePath (PFP (BS.pack $(lift $ BS.unpack bs))) :: AbstractFilePath |]
#endif
#if MIN_VERSION_template_haskell(2,17,0)
  liftTyped = TH.unsafeCodeCoerce . TH.lift
#elif MIN_VERSION_template_haskell(2,16,0)
  liftTyped = TH.unsafeTExpCoerce . TH.lift
#endif

