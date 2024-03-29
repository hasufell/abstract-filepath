{-# LANGUAGE CPP #-}
{- |
   Module      :  System.Win32.SymbolicLink
   Copyright   :  2012 shelarcy
   License     :  BSD-style

   Maintainer  :  shelarcy@gmail.com
   Stability   :  Provisional
   Portability :  Non-portable (Win32 API)

   Handling symbolic link using Win32 API. [Vista of later and desktop app only]

   Note: When using the createSymbolicLink* functions without the
   SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE flag, you should worry about UAC
   (User Account Control) when use this module's function in your application:

     * require to use 'Run As Administrator' to run your application.

     * or modify your application's manifect file to add
       \<requestedExecutionLevel level='requireAdministrator' uiAccess='false'/\>.

   Starting from Windows 10 version 1703 (Creators Update), after enabling
   Developer Mode, users can create symbolic links without requiring the
   Administrator privilege in the current process. Supply a 'True' flag in
   addition to the target and link name to enable this behavior.
-}
module System.Win32.WindowsString.SymbolicLink
  ( SymbolicLinkFlags
  , sYMBOLIC_LINK_FLAG_FILE
  , sYMBOLIC_LINK_FLAG_DIRECTORY
  , sYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE
  , createSymbolicLink
  , createSymbolicLink'
  , createSymbolicLinkFile
  , createSymbolicLinkDirectory
  ) where

import System.Win32.SymbolicLink ( SymbolicLinkFlags )
import Data.Bits ((.|.))
import System.Win32.WindowsString.Types
import System.Win32.WindowsString.File ( failIfFalseWithRetry_ )
import AFP.AbstractFilePath.Windows
import Unsafe.Coerce (unsafeCoerce)

##include "windows_cconv.h"

type SymbolicLinkFlags_ = DWORD

#{enum SymbolicLinkFlags_,
 , sYMBOLIC_LINK_FLAG_FILE      = 0x0
 , sYMBOLIC_LINK_FLAG_DIRECTORY = 0x1
 , sYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE = 0x2
}

-- | createSymbolicLink* functions don't check that file is exist or not.
--
-- NOTE: createSymbolicLink* functions are /flipped arguments/ to provide compatibility for Unix,
-- except 'createSymbolicLink''.
--
-- If you want to create symbolic link by Windows way, use 'createSymbolicLink'' instead.
createSymbolicLink :: WindowsFilePath -- ^ Target file path
                   -> WindowsFilePath -- ^ Symbolic link name
                   -> SymbolicLinkFlags -> IO ()
createSymbolicLink = flip createSymbolicLink'

createSymbolicLinkFile :: WindowsFilePath -- ^ Target file path
                       -> WindowsFilePath -- ^ Symbolic link name
                       -> Bool -- ^ Create the symbolic link with the unprivileged mode
                       -> IO ()
createSymbolicLinkFile target link unprivileged =
  createSymbolicLink'
    link
    target
    ( if unprivileged
        then sYMBOLIC_LINK_FLAG_FILE .|. sYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE
        else sYMBOLIC_LINK_FLAG_FILE
    )

createSymbolicLinkDirectory :: WindowsFilePath -- ^ Target file path
                            -> WindowsFilePath -- ^ Symbolic link name
                            -> Bool -- ^ Create the symbolic link with the unprivileged mode
                            -> IO ()
createSymbolicLinkDirectory target link unprivileged =
  createSymbolicLink'
    link
    target
    ( if unprivileged
        then
          sYMBOLIC_LINK_FLAG_DIRECTORY
            .|. sYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE
        else sYMBOLIC_LINK_FLAG_DIRECTORY
    )

createSymbolicLink' :: WindowsFilePath -- ^ Symbolic link name
                    -> WindowsFilePath -- ^ Target file path
                    -> SymbolicLinkFlags -> IO ()
createSymbolicLink' link target flag = do
    withTString link $ \c_link ->
      withTString target $ \c_target ->
        failIfFalseWithRetry_ (unwords ["CreateSymbolicLink",show link,show target]) $
          c_CreateSymbolicLink c_link c_target (unsafeCoerce flag)

foreign import WINDOWS_CCONV unsafe "windows.h CreateSymbolicLinkW"
  c_CreateSymbolicLink :: LPTSTR -> LPTSTR -> SymbolicLinkFlags_ -> IO BOOL
