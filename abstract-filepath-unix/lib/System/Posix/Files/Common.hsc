{-# LANGUAGE Trustworthy #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Files.Common
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- Functions defined by the POSIX standards for manipulating and querying the
-- file system. Names of underlying POSIX functions are indicated whenever
-- possible. A more complete documentation of the POSIX functions together
-- with a more detailed description of different error conditions are usually
-- available in the system's manual pages or from
-- <http://www.unix.org/version3/online.html> (free registration required).
--
-- When a function that calls an underlying POSIX function fails, the errno
-- code is converted to an 'IOError' using 'Foreign.C.Error.errnoToIOError'.
-- For a list of which errno codes may be generated, consult the POSIX
-- documentation for the underlying function.
--
-----------------------------------------------------------------------------

#include "HsUnix.h"

module System.Posix.Files.Common (
    -- * File status
    FileStatus(..),
    pathVarConst,

    -- * Low level types and functions
#ifdef HAVE_UTIMENSAT
    CTimeSpec(..),
    toCTimeSpec,
    c_utimensat,
#endif
    CTimeVal(..),
    toCTimeVal,
    c_utimes,
#ifdef HAVE_LUTIMES
    c_lutimes,
#endif
  ) where

import System.Posix.Files (PathVar(..))
import System.Posix.Types
import System.IO.Unsafe
import Data.Bits
import Data.Int
import Data.Ratio
import Data.Time.Clock.POSIX (POSIXTime)
import System.Posix.Internals
import Foreign.C
import Foreign.ForeignPtr
#if defined(HAVE_FUTIMES) || defined(HAVE_FUTIMENS)
import Foreign.Marshal (withArray)
#endif
import Foreign.Ptr
import Foreign.Storable


-- -----------------------------------------------------------------------------
-- stat() support

-- | POSIX defines operations to get information, such as owner, permissions,
-- size and access times, about a file. This information is represented by the
-- 'FileStatus' type.
--
-- Note: see @chmod@.
newtype FileStatus = FileStatus (ForeignPtr CStat)

-- -----------------------------------------------------------------------------
-- Setting file times

#if HAVE_UTIMENSAT || HAVE_FUTIMENS
data CTimeSpec = CTimeSpec EpochTime CLong

instance Storable CTimeSpec where
    sizeOf    _ = #size struct timespec
    alignment _ = alignment (undefined :: CInt)
    poke p (CTimeSpec sec nsec) = do
        (#poke struct timespec, tv_sec ) p sec
        (#poke struct timespec, tv_nsec) p nsec
    peek p = do
        sec  <- #{peek struct timespec, tv_sec } p
        nsec <- #{peek struct timespec, tv_nsec} p
        return $ CTimeSpec sec nsec

toCTimeSpec :: POSIXTime -> CTimeSpec
toCTimeSpec t = CTimeSpec (CTime sec) (truncate $ 10^(9::Int) * frac)
  where
    (sec, frac) = if (frac' < 0) then (sec' - 1, frac' + 1) else (sec', frac')
    (sec', frac') = properFraction $ toRational t
#endif

#ifdef HAVE_UTIMENSAT
foreign import ccall unsafe "utimensat"
    c_utimensat :: CInt -> CString -> Ptr CTimeSpec -> CInt -> IO CInt
#endif

#if HAVE_FUTIMENS
foreign import ccall unsafe "futimens"
    c_futimens :: CInt -> Ptr CTimeSpec -> IO CInt
#endif

data CTimeVal = CTimeVal CLong CLong

instance Storable CTimeVal where
    sizeOf    _ = #size struct timeval
    alignment _ = alignment (undefined :: CInt)
    poke p (CTimeVal sec usec) = do
        (#poke struct timeval, tv_sec ) p sec
        (#poke struct timeval, tv_usec) p usec
    peek p = do
        sec  <- #{peek struct timeval, tv_sec } p
        usec <- #{peek struct timeval, tv_usec} p
        return $ CTimeVal sec usec

toCTimeVal :: POSIXTime -> CTimeVal
toCTimeVal t = CTimeVal sec (truncate $ 10^(6::Int) * frac)
  where
    (sec, frac) = if (frac' < 0) then (sec' - 1, frac' + 1) else (sec', frac')
    (sec', frac') = properFraction $ toRational t

foreign import ccall unsafe "utimes"
    c_utimes :: CString -> Ptr CTimeVal -> IO CInt

#ifdef HAVE_LUTIMES
foreign import ccall unsafe "lutimes"
    c_lutimes :: CString -> Ptr CTimeVal -> IO CInt
#endif

#if HAVE_FUTIMES
foreign import ccall unsafe "futimes"
    c_futimes :: CInt -> Ptr CTimeVal -> IO CInt
#endif


-- -----------------------------------------------------------------------------
-- pathconf()/fpathconf() support

pathVarConst :: PathVar -> CInt
pathVarConst v = case v of
        LinkLimit                       -> (#const _PC_LINK_MAX)
        InputLineLimit                  -> (#const _PC_MAX_CANON)
        InputQueueLimit                 -> (#const _PC_MAX_INPUT)
        FileNameLimit                   -> (#const _PC_NAME_MAX)
        PathNameLimit                   -> (#const _PC_PATH_MAX)
        PipeBufferLimit                 -> (#const _PC_PIPE_BUF)
        SetOwnerAndGroupIsRestricted    -> (#const _PC_CHOWN_RESTRICTED)
        FileNamesAreNotTruncated        -> (#const _PC_NO_TRUNC)
        VDisableChar                    -> (#const _PC_VDISABLE)

#ifdef _PC_SYNC_IO
        SyncIOAvailable         -> (#const _PC_SYNC_IO)
#else
        SyncIOAvailable         -> error "_PC_SYNC_IO not available"
#endif

#ifdef _PC_ASYNC_IO
        AsyncIOAvailable        -> (#const _PC_ASYNC_IO)
#else
        AsyncIOAvailable        -> error "_PC_ASYNC_IO not available"
#endif

#ifdef _PC_PRIO_IO
        PrioIOAvailable         -> (#const _PC_PRIO_IO)
#else
        PrioIOAvailable         -> error "_PC_PRIO_IO not available"
#endif

#if _PC_FILESIZEBITS
        FileSizeBits            -> (#const _PC_FILESIZEBITS)
#else
        FileSizeBits            -> error "_PC_FILESIZEBITS not available"
#endif

#if _PC_SYMLINK_MAX
        SymbolicLinkLimit       -> (#const _PC_SYMLINK_MAX)
#else
        SymbolicLinkLimit       -> error "_PC_SYMLINK_MAX not available"
#endif
