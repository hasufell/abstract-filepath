{-# LANGUAGE CPP, RankNTypes, UnliftedFFITypes #-}

module AbstractFilePath.Internal where

import AbstractFilePath.Internal.Decode (decodeUtf16LE, decodeUtf16LEWith, decodeUtf8, decodeUtf8With)
import AbstractFilePath.Internal.Encode (encodeUtf16LE, encodeUtf8)

import GHC.Exts ( IsString(..) )

import qualified Data.ByteString.Short as BS
import Data.Text.Encoding.Error (lenientDecode)


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
-- On unix this encodes as UTF8, which is a good guess. Note that
-- filenames on unix are encoding agnostic char arrays.
toAbstractFilePath :: String -> AbstractFilePath
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
toAbstractFilePath = AbstractFilePath . WFP . encodeUtf16LE
#else
toAbstractFilePath = AbstractFilePath . PFP . encodeUtf8
#endif


-- | Partial unicode friendly decoding.
--
-- On windows this decodes as UTF16 (which is the expected filename encoding).
-- On unix this decodes as UTF8 (which is a good guess). Note that
-- filenames on unix are encoding agnostic char arrays.
--
-- Throws a 'UnicodeException' if decoding fails.
fromAbstractFilePath :: AbstractFilePath -> String
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
fromAbstractFilePath (AbstractFilePath (WFP ba)) = decodeUtf16LE ba
#else
fromAbstractFilePath (AbstractFilePath (PFP ba)) = decodeUtf8 ba
#endif


-- | Type representing filenames/pathnames.
--
-- On Windows, filepaths are expected to be in UTF16 as passed
-- to syscalls.
--
-- On unix, filepathes don't have a known encoding (although they
-- are often interpreted as UTF8) and are passed
-- as char arrays to syscalls.
--
-- This type uses an internal representation of unpinned
-- 'ShortByteString' for efficiency.
--
-- The 'Eq' and 'Ord' instances are low-level and don't know about
-- upper/lower filename shenanigans. They compare the bytes that
-- are passed to syscalls.
--
-- 'IsString' calls 'toAbstractFilePath'.
newtype AbstractFilePath = AbstractFilePath PlatformFilePath
  deriving (Eq, Ord)

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
#else
    mempty      = AbstractFilePath (PFP BS.empty)
#endif

instance Semigroup AbstractFilePath where 
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
    (AbstractFilePath (WFP a)) <> (AbstractFilePath (WFP b))
      = AbstractFilePath (WFP (a <> b))
#else
    (AbstractFilePath (PFP a)) <> (AbstractFilePath (PFP b))
      = AbstractFilePath (PFP (a <> b))
#endif
