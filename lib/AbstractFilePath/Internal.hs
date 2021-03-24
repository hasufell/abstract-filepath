{-# LANGUAGE CPP, RankNTypes, UnliftedFFITypes #-}

module AbstractFilePath.Internal where

import AbstractFilePath.Internal.Decode (decodeUtf16LE, decodeUtf8)

import GHC.Exts

import qualified Data.ByteString.Short as BS


-- Using unpinned bytearrays to avoid Heap fragmentation and
-- which are reasonably cheap to pass to FFI calls
-- wrapped with typeclass-friendly types allowing to avoid CPP
-- 
-- Note that, while unpinned bytearrays incur a memcpy on each
-- FFI call, this overhead is generally much preferable to
-- the memory fragmentation of pinned bytearrays
data WindowsFilePath = WFP BS.ShortByteString -- UTF16 data
data PosixFilePath   = PFP BS.ShortByteString -- char[] data as passed to syscalls

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
type PlatformFilePath = WindowsFilePath
#else
type PlatformFilePath = PosixFilePath
#endif

-- | Total Unicode-friendly encoding
--
-- On windows this decodes as UTF16, which is expected.
-- On unix this decodes as UTF8, which is a good guess. Note that
-- filepaths on unix are encoding agnostic char arrays.
toAbstractFilePath :: String -> AbstractFilePath
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
toAbstractFilePath = undefined
#else
toAbstractFilePath = undefined
#endif


-- | On windows this decodes as UTF16 (which is the expected filename encoding).
-- On unix this decodes as UTF8 (which is a good guess). Note that
-- filepathes on unix are encoding agnostic char arrays.
--
-- Throws a 'UnicodeException' if decoding fails.
fromAbstractFilePath :: AbstractFilePath -> String
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
fromAbstractFilePath (AbstractFilePath (WFP ba)) = decodeUtf16LE ba
#else
fromAbstractFilePath (AbstractFilePath (PFP ba)) = decodeUtf8 ba
#endif

fromAbstractFilePath' :: AbstractFilePath -> String
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
fromAbstractFilePath' (AbstractFilePath (WFP ba)) = decodeUtf16LE ba
#else
fromAbstractFilePath' (AbstractFilePath (PFP ba)) = decodeUtf8 ba
#endif



-- | Type representing filenames/pathnames.
--
-- On Windows, filepaths are expected to be in UTF16 as passed
-- to syscalls.
--
-- On unix, filepathes don't have a known encoding and are passed
-- as char arrays to syscalls.
newtype AbstractFilePath = AbstractFilePath PlatformFilePath

instance IsString AbstractFilePath where 
    fromString = toAbstractFilePath

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
