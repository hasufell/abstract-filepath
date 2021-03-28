-- |
-- Module      :  AbstractFilePath
-- Copyright   :  © 2021 Julian Ospald
-- License     :  MIT
--
-- Maintainer  :  Julian Ospald <hasufell@posteo.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- An implementation of the <https://gitlab.haskell.org/ghc/ghc/-/wikis/proposal/abstract-file-path Abstract FilePath Proposal>,
-- which aims to supersede @type FilePath = String@ for various reasons:
--
-- 1. it is more efficient (uses unpinned 'ShortByteString' under the hood)
-- 2. is more type-safe (not a type synonym, but a newtype)
-- 3. avoids round-tripping issues, by not converting to String (which loses the encoding)
--
-- It is important to know that filenames\/filepaths have different representations across platforms:
--
-- - On /Windows/, filepaths are expected to be in UTF16 as passed to
--   syscalls. This invariant is maintained by 'AbstractFilePath'.
-- - On /Unix/, filepaths don't have a predefined encoding (although they
--   are often interpreted as UTF8) as per the
--   <https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap03.html#tag_03_170 POSIX specification>
--   and are passed as @char[]@ to shttps://files.hasufell.de/jule/abstract-filepath/AbstractFilePath.htmlyscalls. 'AbstractFilePath' maintains no invariant
--   here. Some functions however, such as 'toAbstractFilePath', may expect
--   or produce UTF8.
--
-- Note that further filesystem specific restrictions may apply on
-- all platforms. This library makes no attempt at satisfying these
-- restrictions. Library users may need to account for that, depending
-- on what filesystems they want to support.
--
-- It is advised to follow these principles when dealing with filepaths\/filenames:
--
-- 1. Avoid interpreting filenames that the OS returns, unless absolutely necessary.
-- 2. When interpreting OS returned filenames consider that these might not be UTF8 on /unix/
--    or at worst don't have an ASCII compatible encoding. Some strategies here involve looking
--    up the current locale and use that for decoding ('fromAbstractFilePath'' does this).
--    Otherwise it can be reasonable to assume UTF8 on unix ('fromAbstractFilePath' does that) if your application specifically
--    mentions that it requires a UTF8 compatible system. These things should be documented.
-- 3. When dealing with user input (e.g. on the command line) on /unix/ as e.g. @String@ the input
--    encoding is lost. The output encoding (e.g. how we write a filename to disk) can then
--    either follow the current locale again ('toAbstractFilePath'') or a fixed encoding
--    ('toAbstractFilePath'). The decision should be clearly documented. If the input is in the
--    form of a @ByteString@, then 'fromByteString' may be of interest, unless the input needs further
--    interpretation.

module AbstractFilePath
  (
  -- * Types
    AbstractFilePath

  -- * Construction
  , toAbstractFilePath
  , toAbstractFilePath'
  , fromByteString

  -- * Deconstruction
  , fromAbstractFilePath
  , fromAbstractFilePath'
  )
where

import AbstractFilePath.Internal ( AbstractFilePath, toAbstractFilePath, toAbstractFilePath', fromByteString, fromAbstractFilePath, fromAbstractFilePath' )
