{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module AFP.OsString.Internal.Types where

import {-# SOURCE #-} AFP.OsString.Internal
    ( toOsString )

import AFP.Data.ByteString.Short.Decode
    ( decodeUtf16LE, decodeUtf8 )
import AFP.Data.ByteString.Short.Encode
    ( encodeUtf16LE, encodeUtf8 )

import AFP.Data.Word16
import Data.Word8

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

-- Using unpinned bytearrays to avoid Heap fragmentation and
-- which are reasonably cheap to pass to FFI calls
-- wrapped with typeclass-friendly types allowing to avoid CPP
-- 
-- Note that, while unpinned bytearrays incur a memcpy on each
-- FFI call, this overhead is generally much preferable to
-- the memory fragmentation of pinned bytearrays

-- | Commonly used windows string as UTF16 bytes.
newtype WindowsString = WS { unWFP :: BS.ShortByteString }
  deriving (Eq, Ord, Semigroup, Monoid)

instance Lift WindowsString where
  lift (WS bs)
    = [| (WS (BS.pack $(lift $ BS.unpack bs))) :: WindowsString |]
#if MIN_VERSION_template_haskell(2,17,0)
  liftTyped = TH.unsafeCodeCoerce . TH.lift
#elif MIN_VERSION_template_haskell(2,16,0)
  liftTyped = TH.unsafeTExpCoerce . TH.lift
#endif

-- | Commonly used Posix string as uninterpreted @char[]@
-- array.
newtype PosixString   = PS { unPFP :: BS.ShortByteString }
  deriving (Eq, Ord, Semigroup, Monoid)

instance Lift PosixString where
  lift (PS bs)
    = [| (PS (BS.pack $(lift $ BS.unpack bs))) :: PosixString |]
#if MIN_VERSION_template_haskell(2,17,0)
  liftTyped = TH.unsafeCodeCoerce . TH.lift
#elif MIN_VERSION_template_haskell(2,16,0)
  liftTyped = TH.unsafeTExpCoerce . TH.lift
#endif

instance Show WindowsString where
  show (WS bs) = ('\"': decodeUtf16LE bs) <> "\""

instance Show PosixString where
  show (PS bs) = ('\"': decodeUtf8 bs) <> "\""

instance IsString WindowsString where 
    fromString = WS . encodeUtf16LE

instance IsString PosixString where 
    fromString = PS . encodeUtf8

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
type PlatformString = WindowsString
#else
type PlatformString = PosixString
#endif

newtype WindowsWord = WW Word16
  deriving (Eq, Ord, Show)
newtype PosixWord   = PW Word8
  deriving (Eq, Ord, Show)

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
type PlatformWord = WindowsWord
#else
type PlatformWord = PosixWord
#endif


-- | Newtype representing operating system specific strings.
--
-- Internally this is either 'WindowsFilePath' or 'PosixFilePath',
-- depending on the platform. Both use unpinned
-- 'ShortByteString' for efficiency and correctness.
--
-- The constructor is only exported via "AFP.OsString.Internal.Types", since
-- dealing with the internals isn't generally recommended, but supported
-- in case you need to write platform specific code, such as the implementation
-- of 'fromAbstractFilePath'.
newtype OsString = OsString PlatformString

-- | Byte equality of the internal representation.
instance Eq OsString where
  (OsString a) == (OsString b) = a == b

-- | Byte ordering of the internal representation.
instance Ord OsString where
  compare (OsString a) (OsString b) = compare a b

-- | Encodes as UTF16 on windows and UTF8 on unix.
instance IsString OsString where 
    fromString = toOsString

-- | \"String-Concatenation\" for 'AbstractFilePath'. This is __not__ the same
-- as '(</>)'.
instance Monoid OsString where 
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
    mempty      = OsString (WS BS.empty)
    mappend (OsString (WS a)) (OsString (WS b))
      = OsString (WS (mappend a b))
#else
    mempty      = OsString (PS BS.empty)
    mappend (OsString (PS a)) (OsString (PS b))
      = OsString (PS (mappend a b))
#endif
#if MIN_VERSION_base(4,11,0)
instance Semigroup OsString where 
    (<>) = mappend
#endif


instance Lift OsString where
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
  lift (OsString (WS bs))
    = [| OsString (WS (BS.pack $(lift $ BS.unpack bs))) :: OsString |]
#else
  lift (OsString (PS bs))
    = [| OsString (PS (BS.pack $(lift $ BS.unpack bs))) :: OsString |]
#endif
#if MIN_VERSION_template_haskell(2,17,0)
  liftTyped = TH.unsafeCodeCoerce . TH.lift
#elif MIN_VERSION_template_haskell(2,16,0)
  liftTyped = TH.unsafeTExpCoerce . TH.lift
#endif

instance Show OsString where
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
  show (OsString (WS bs)) = ('\"': decodeUtf16LE bs) <> "\""
#else
  show (OsString (PS bs)) = ('\"': decodeUtf8 bs) <> "\""
#endif


-- | Newtype representing operating system Word with respect to
-- the encoding. On Windows, this is 'Word16', on POSIX 'Word8'.
newtype OsWord = OsWord PlatformWord
  deriving Show

-- | Byte equality of the internal representation.
instance Eq OsWord where
  (OsWord a) == (OsWord b) = a == b

-- | Byte ordering of the internal representation.
instance Ord OsWord where
  compare (OsWord a) (OsWord b) = compare a b
