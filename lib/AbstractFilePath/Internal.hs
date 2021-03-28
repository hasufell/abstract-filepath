{-# LANGUAGE CPP, RankNTypes, UnliftedFFITypes #-}

module AbstractFilePath.Internal where

import AbstractFilePath.Internal.Decode (decodeUtf16LE, decodeUtf16LEWith, decodeUtf16LE', decodeUtf16LE'', decodeUtf8, decodeUtf8With, decodeUtf8')
import AbstractFilePath.Internal.Encode (encodeUtf16LE, encodeUtf8)

import Data.ByteString ( ByteString )
import GHC.Exts ( IsString(..) )
import GHC.IO.Encoding ( getFileSystemEncoding )

import Control.Monad.Catch (MonadThrow, throwM)

import Data.Text.Encoding.Error (lenientDecode)

import qualified Data.ByteString.Short as BS
import qualified Data.Text.Encoding as E
import qualified GHC.Foreign as GHC
import Control.Exception (throwIO)


-- Using unpinned bytearrays to avoid Heap fragmentation and
-- which are reasonably cheap to pass to FFI calls
-- wrapped with typeclass-friendly types allowing to avoid CPP
-- 
-- Note that, while unpinned bytearrays incur a memcpy on each
-- FFI call, this overhead is generally much preferable to
-- the memory fragmentation of pinned bytearrays
data WindowsFilePath = WFP BS.ShortByteString -- UTF16 data
  deriving (Eq, Ord)
data PosixFilePath   = PFP BS.ShortByteString -- char[] data as passed to syscalls
  deriving (Eq, Ord)

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
type PlatformFilePath = WindowsFilePath
#else
type PlatformFilePath = PosixFilePath
#endif

-- | Total Unicode-friendly encoding.
--
-- On windows this encodes as UTF16, which is expected.
-- On unix this encodes as UTF8, which is a good guess.
toAbstractFilePath :: String -> AbstractFilePath
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
toAbstractFilePath = AbstractFilePath . WFP . encodeUtf16LE
#else
toAbstractFilePath = AbstractFilePath . PFP . encodeUtf8
#endif


-- | Like 'toAbstractFilePath', except on unix this uses the current
-- locale for encoding instead of always UTF8. Looking up the locale
-- requires IO.
toAbstractFilePath' :: String -> IO AbstractFilePath
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
toAbstractFilePath' = AbstractFilePath . WFP . encodeUtf16LE
#else
toAbstractFilePath' str = do
  enc <- getFileSystemEncoding
  cstr <- GHC.newCString enc str
  AbstractFilePath . PFP <$> BS.packCString cstr
#endif


-- | Partial unicode friendly decoding.
--
-- On windows this decodes as UTF16 (which is the expected filename encoding).
-- On unix this decodes as UTF8 (which is a good guess). Note that
-- filenames on unix are encoding agnostic char arrays.
--
-- Note that filenames of different encodings may have the same @String@
-- representation, although they're not the same byte-wise.
fromAbstractFilePath :: AbstractFilePath -> String
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
fromAbstractFilePath (AbstractFilePath (WFP ba)) = decodeUtf16LE ba
#else
fromAbstractFilePath (AbstractFilePath (PFP ba)) = decodeUtf8 ba
#endif


-- | Like 'fromAbstractFilePath', except on unix this uses the current
-- locale for decoding instead of always UTF8. Looking up the locale
-- requires IO.
fromAbstractFilePath' :: AbstractFilePath -> IO String
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
fromAbstractFilePath' (AbstractFilePath (WFP ba)) = either throwIO pure $ decodeUtf16LE' ba
#else
fromAbstractFilePath' (AbstractFilePath (PFP ba)) = BS.useAsCString ba $ \fp ->
  getFileSystemEncoding >>= \enc -> GHC.peekCString enc fp
#endif


-- | Constructs an @AbstractFilePath@ from a ByteString.
--
-- On windows, this ensures valid UTF16, on unix it is passed unchanged/unchecked.
--
-- Throws 'UnicodeException' on invalid UTF16 on windows.
fromByteString :: MonadThrow m
               => ByteString
               -> m AbstractFilePath
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
fromByteString bs =
  either throwM (const . pure . AbstractFilePath . WFP . BS.toShort $ bs) $ decodeUtf16LE'' bs
#else
fromByteString = pure . AbstractFilePath . PFP . BS.toShort
#endif


-- | Type representing filenames\/pathnames across platforms.
--
-- Uses an internal representation of unpinned
-- 'ShortByteString' for efficiency and correctness. If you need access to
-- it for low-level purposes or writing platform-specific code,
-- import "AbstractFilePath.Internal", which exposes the
-- private constructors.
--
-- As an example of platform specific code, consider the implementation of
-- 'fromAbstractFilePath'.
newtype AbstractFilePath = AbstractFilePath PlatformFilePath

-- | Byte equality of the internal representation.
instance Eq AbstractFilePath where
  (AbstractFilePath a) == (AbstractFilePath b) = a == b

-- | Byte ordering of the internal representation.
instance Ord AbstractFilePath where
  compare (AbstractFilePath a) (AbstractFilePath b) = compare a b

-- | Calls 'toAbstractFilePath'. This instance is total.
instance IsString AbstractFilePath where 
    fromString = toAbstractFilePath

instance Show AbstractFilePath where
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
  show (AbstractFilePath (WFP ba)) = decodeUtf16LEWith lenientDecode ba
#else
  show (AbstractFilePath (PFP ba)) = decodeUtf8With lenientDecode ba
#endif

-- | \"String-Concatenation\" for 'AbstractFilePaths'
--
-- This allows to write forward-compatible code for Haskell2010 'FilePath`s
--
-- E.g. code can be written (assuming `-XOverloadedStrings`) like
--
-- > tarfname = basedir </> "ghc-" <> ver <> "~" <> gitid <.> "tar.xz"
--
-- That has the same semantics with pre-AFPP and post-AFPP 'FilePath's
--
-- NB: 'mappend' is *not* the same as '(</>)', but rather matches the semantics for pre-AFPP 'FilePaths'
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
