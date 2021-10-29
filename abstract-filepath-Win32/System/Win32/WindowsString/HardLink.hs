{-# LANGUAGE CPP #-}
{- |
   Module      :  System.Win32.HardLink
   Copyright   :  2013 shelarcy
   License     :  BSD-style

   Maintainer  :  shelarcy@gmail.com
   Stability   :  Provisional
   Portability :  Non-portable (Win32 API)

   Handling hard link using Win32 API. [NTFS only]

   Note: You should worry about file system type when use this module's function in your application:

     * NTFS only supprts this functionality.

     * ReFS doesn't support hard link currently.
-}
module System.Win32.WindowsString.HardLink
  ( createHardLink
  , createHardLink'
  ) where

import System.Win32.WindowsString.File   ( LPSECURITY_ATTRIBUTES, failIfFalseWithRetry_ )
import System.Win32.WindowsString.String ( LPCTSTR, withTString )
import System.Win32.WindowsString.Types  ( BOOL, nullPtr )
import AFP.AbstractFilePath.Windows

#include "windows_cconv.h"

-- | NOTE: createHardLink is /flipped arguments/ to provide compatibility for Unix.
-- 
-- If you want to create hard link by Windows way, use 'createHardLink'' instead.
createHardLink :: WindowsFilePath -- ^ Target file path
               -> WindowsFilePath -- ^ Hard link name
               -> IO ()
createHardLink = flip createHardLink'

createHardLink' :: WindowsFilePath -- ^ Hard link name
                -> WindowsFilePath -- ^ Target file path
                -> IO ()
createHardLink' link target =
   withTString target $ \c_target ->
   withTString link   $ \c_link ->
        failIfFalseWithRetry_ (unwords ["CreateHardLinkW",show link,show target]) $
          c_CreateHardLink c_link c_target nullPtr

foreign import WINDOWS_CCONV unsafe "windows.h CreateHardLinkW"
  c_CreateHardLink :: LPCTSTR -- ^ Hard link name
                   -> LPCTSTR -- ^ Target file path
                   -> LPSECURITY_ATTRIBUTES -- ^ This parameter is reserved. You should pass just /nullPtr/.
                   -> IO BOOL

