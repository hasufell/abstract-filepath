-----------------------------------------------------------------------------
-- |
-- Module      :  System.Win32.DLL
-- Copyright   :  (c) Alastair Reid, 1997-2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Esa Ilari Vuokko <ei@vuokko.info>
-- Stability   :  provisional
-- Portability :  portable
--
-- A collection of FFI declarations for interfacing with Win32.
--
-----------------------------------------------------------------------------

module System.Win32.WindowsString.DLL
    ( module System.Win32.WindowsString.DLL
    , module System.Win32.DLL
    ) where

import System.Win32.DLL hiding
  ( disableThreadLibraryCalls
    , freeLibrary
    , getModuleFileName
    , getModuleHandle
    , getProcAddress
    , loadLibrary
    , loadLibraryEx
    , setDllDirectory
    , lOAD_LIBRARY_AS_DATAFILE
    , lOAD_WITH_ALTERED_SEARCH_PATH
  )
import System.Win32.WindowsString.Types

import Foreign
import Foreign.C
import Data.Maybe (fromMaybe)
import AFP.OsString.Windows

##include "windows_cconv.h"

#include <windows.h>

disableThreadLibraryCalls :: HMODULE -> IO ()
disableThreadLibraryCalls hmod =
  failIfFalse_ "DisableThreadLibraryCalls" $ c_DisableThreadLibraryCalls hmod
foreign import WINDOWS_CCONV unsafe "windows.h DisableThreadLibraryCalls"
  c_DisableThreadLibraryCalls :: HMODULE -> IO Bool

freeLibrary :: HMODULE -> IO ()
freeLibrary hmod =
  failIfFalse_ "FreeLibrary" $ c_FreeLibrary hmod
foreign import WINDOWS_CCONV unsafe "windows.h FreeLibrary"
  c_FreeLibrary :: HMODULE -> IO Bool

getModuleFileName :: HMODULE -> IO WindowsString
getModuleFileName hmod =
  allocaArray 512 $ \ c_str -> do
  failIfFalse_ "GetModuleFileName" $ c_GetModuleFileName hmod c_str 512
  peekTString c_str
foreign import WINDOWS_CCONV unsafe "windows.h GetModuleFileNameW"
  c_GetModuleFileName :: HMODULE -> LPTSTR -> Int -> IO Bool

getModuleHandle :: Maybe WindowsString -> IO HMODULE
getModuleHandle mb_name =
  maybeWith withTString mb_name $ \ c_name ->
  failIfNull "GetModuleHandle" $ c_GetModuleHandle c_name
foreign import WINDOWS_CCONV unsafe "windows.h GetModuleHandleW"
  c_GetModuleHandle :: LPCTSTR -> IO HMODULE

getProcAddress :: HMODULE -> String -> IO Addr
getProcAddress hmod procname =
  withCAString procname $ \ c_procname ->
  failIfNull "GetProcAddress" $ c_GetProcAddress hmod c_procname

foreign import WINDOWS_CCONV unsafe "windows.h GetProcAddress"
  c_GetProcAddress :: HMODULE -> LPCSTR -> IO Addr

loadLibrary :: WindowsString -> IO HINSTANCE
loadLibrary name =
  withTString name $ \ c_name ->
  failIfNull "LoadLibrary" $ c_LoadLibrary c_name
foreign import WINDOWS_CCONV unsafe "windows.h LoadLibraryW"
  c_LoadLibrary :: LPCTSTR -> IO HINSTANCE

#{enum LoadLibraryFlags,
 , lOAD_LIBRARY_AS_DATAFILE      = LOAD_LIBRARY_AS_DATAFILE
 , lOAD_WITH_ALTERED_SEARCH_PATH = LOAD_WITH_ALTERED_SEARCH_PATH
 }

loadLibraryEx :: WindowsString -> HANDLE -> LoadLibraryFlags -> IO HINSTANCE
loadLibraryEx name h flags =
  withTString name $ \ c_name ->
  failIfNull "LoadLibraryEx" $ c_LoadLibraryEx c_name h flags
foreign import WINDOWS_CCONV unsafe "windows.h LoadLibraryExW"
  c_LoadLibraryEx :: LPCTSTR -> HANDLE -> LoadLibraryFlags -> IO HINSTANCE

setDllDirectory :: Maybe WindowsString -> IO ()
setDllDirectory name =
  maybeWith withTString name $ \ c_name -> do
    let nameS = name >>= fromPlatformString
    failIfFalse_ (unwords ["SetDllDirectory", fromMaybe "NULL" nameS]) $ c_SetDllDirectory c_name

foreign import WINDOWS_CCONV unsafe "windows.h SetDllDirectoryW"
  c_SetDllDirectory :: LPTSTR -> IO BOOL
