-----------------------------------------------------------------------------
-- |
-- Module      :  System.Win32.Path
-- Copyright   :  (c) Tamar Christina, 1997-2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Tamar Christina <tamar@zhox.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- A collection of FFI declarations for interfacing with Win32.
--
-----------------------------------------------------------------------------

module System.Win32.WindowsString.Path (
   filepathRelativePathTo
 , pathRelativePathTo
 ) where

import System.Win32.WindowsString.Types
import System.Win32.WindowsString.File
import AFP.AbstractFilePath.Windows

import Foreign

##include "windows_cconv.h"

#include <windows.h>

filepathRelativePathTo :: WindowsFilePath -> WindowsFilePath -> IO WindowsFilePath
filepathRelativePathTo from to =
  withTString from $ \p_from ->
  withTString to   $ \p_to   ->
  allocaArray ((#const MAX_PATH) * (#size TCHAR)) $ \p_AbsPath -> do
    _ <- failIfZero "PathRelativePathTo" (c_pathRelativePathTo p_AbsPath p_from fILE_ATTRIBUTE_DIRECTORY
                                                                         p_to   fILE_ATTRIBUTE_NORMAL)
    path <- peekTString p_AbsPath
    _ <- localFree p_AbsPath
    return path

pathRelativePathTo :: WindowsFilePath -> FileAttributeOrFlag -> WindowsFilePath -> FileAttributeOrFlag -> IO WindowsFilePath
pathRelativePathTo from from_attr to to_attr =
  withTString from $ \p_from ->
  withTString to   $ \p_to   ->
  allocaArray ((#const MAX_PATH) * (#size TCHAR)) $ \p_AbsPath -> do
    _ <- failIfZero "PathRelativePathTo" (c_pathRelativePathTo p_AbsPath p_from from_attr
                                                                         p_to   to_attr)
    path <- peekTString p_AbsPath
    _ <- localFree p_AbsPath
    return path

foreign import WINDOWS_CCONV unsafe "Shlwapi.h PathRelativePathToW"
         c_pathRelativePathTo :: LPTSTR -> LPCTSTR -> DWORD -> LPCTSTR -> DWORD -> IO UINT
