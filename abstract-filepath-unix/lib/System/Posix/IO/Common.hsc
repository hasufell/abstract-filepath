{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Safe #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.IO.Common
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-----------------------------------------------------------------------------

module System.Posix.IO.Common (
    open_,
  ) where

import System.IO
import System.IO.Error
import System.Posix.IO
import System.Posix.Types
import qualified System.Posix.Internals as Base

import Foreign
import Foreign.C

import GHC.IO.Handle.Internals
import GHC.IO.Handle.Types
import qualified GHC.IO.FD as FD
import qualified GHC.IO.Handle.FD as FD
import GHC.IO.Exception
import Data.Typeable (cast)

#include "HsUnix.h"

-- |Open and optionally create this file.  See 'System.Posix.Files'
-- for information on how to use the 'FileMode' type.
open_  :: CString
       -> OpenMode
       -> Maybe FileMode -- ^Just x => creates the file with the given modes, Nothing => the file must exist.
       -> OpenFileFlags
       -> IO Fd
open_ str how maybe_mode (OpenFileFlags appendFlag exclusiveFlag nocttyFlag
                                nonBlockFlag truncateFlag) = do
    fd <- c_open str all_flags mode_w
    return (Fd fd)
  where
    all_flags  = creat .|. flags .|. open_mode

    flags =
       (if appendFlag    then (#const O_APPEND)   else 0) .|.
       (if exclusiveFlag then (#const O_EXCL)     else 0) .|.
       (if nocttyFlag    then (#const O_NOCTTY)   else 0) .|.
       (if nonBlockFlag  then (#const O_NONBLOCK) else 0) .|.
       (if truncateFlag  then (#const O_TRUNC)    else 0)

    (creat, mode_w) = case maybe_mode of
                        Nothing -> (0,0)
                        Just x  -> ((#const O_CREAT), x)

    open_mode = case how of
                   ReadOnly  -> (#const O_RDONLY)
                   WriteOnly -> (#const O_WRONLY)
                   ReadWrite -> (#const O_RDWR)

foreign import capi unsafe "HsUnix.h open"
   c_open :: CString -> CInt -> CMode -> IO CInt
