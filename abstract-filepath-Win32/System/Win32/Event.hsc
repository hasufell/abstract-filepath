-----------------------------------------------------------------------------
-- |
-- Module      :  System.Win32.Event
-- Copyright   :  (c) Esa Ilari Vuokko, 2006
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Esa Ilari Vuokko <ei@vuokko.info>
-- Stability   :  provisional
-- Portability :  portable
--
-- A collection of FFI declarations for interfacing with Win32 event system between
-- processes.
--
-----------------------------------------------------------------------------
module System.Win32.Event
    ( -- * Duplicate options
      DuplicateOption
    , dUPLICATE_CLOSE_SOURCE
    , dUPLICATE_SAME_ACCESS
      -- * Access modes
    , AccessMode
    , eVENT_ALL_ACCESS
    , eVENT_MODIFY_STATE
      -- * Wait results
    , WaitResult
    , wAIT_ABANDONED
    , wAIT_IO_COMPLETION
    , wAIT_OBJECT_0
    , wAIT_TIMEOUT
    , wAIT_FAILED
      -- * Managing events
    , openEvent
    , createEvent
    , duplicateHandle
    , setEvent
    , resetEvent
    , pulseEvent
      -- * Signalling objects
    , signalObjectAndWait
      -- * Waiting on objects
    , waitForSingleObject
    , waitForSingleObjectEx
    , waitForMultipleObjects
    , waitForMultipleObjectsEx
    ) where

import Foreign.Marshal.Alloc     ( alloca )
import Foreign.Marshal.Array     ( withArrayLen )
import Foreign.Marshal.Utils     ( maybeWith, with )
import Foreign.Ptr               ( Ptr, nullPtr )
import Foreign.Storable          ( Storable(..) )
import Graphics.Win32.Misc       ( MilliSeconds )
import System.Win32.File         ( AccessMode, LPSECURITY_ATTRIBUTES, SECURITY_ATTRIBUTES )
import System.Win32.Types        ( LPCTSTR, HANDLE, BOOL, withTString, failIf, failIfFalse_  )
import System.Win32.Word         ( DWORD )

##include "windows_cconv.h"

#include "windows.h"
#include "winuser_compat.h"
#include "alignment.h"

---------------------------------------------------------------------------
-- Enums
---------------------------------------------------------------------------
type DuplicateOption = DWORD
#{enum DuplicateOption,
    , dUPLICATE_CLOSE_SOURCE = DUPLICATE_CLOSE_SOURCE
    , dUPLICATE_SAME_ACCESS  = DUPLICATE_SAME_ACCESS
    }

#{enum AccessMode,
    , eVENT_ALL_ACCESS   = EVENT_ALL_ACCESS
    , eVENT_MODIFY_STATE = EVENT_MODIFY_STATE
    }

type WaitResult = DWORD
#{enum WaitResult,
    , wAIT_ABANDONED     = WAIT_ABANDONED
    , wAIT_IO_COMPLETION = WAIT_IO_COMPLETION
    , wAIT_OBJECT_0      = WAIT_OBJECT_0
    , wAIT_TIMEOUT       = WAIT_TIMEOUT
    , wAIT_FAILED        = WAIT_FAILED
    }

---------------------------------------------------------------------------
-- API in Haskell
---------------------------------------------------------------------------
openEvent :: AccessMode -> Bool -> String -> IO HANDLE
openEvent amode inherit name = withTString name $ \c_name ->
    failIf (==nullPtr) "openEvent: OpenEvent" $ c_OpenEvent (fromIntegral amode) inherit c_name

createEvent :: Maybe SECURITY_ATTRIBUTES -> Bool -> Bool -> String -> IO HANDLE
createEvent msecurity manual initial name = withTString name $ \c_name ->
    maybeWith with msecurity $ \c_sec ->
        failIf (==nullPtr) "createEvent: CreateEvent" $ c_CreateEvent c_sec manual initial c_name

duplicateHandle :: HANDLE -> HANDLE -> HANDLE -> AccessMode -> Bool -> DuplicateOption -> IO HANDLE
duplicateHandle srcProccess srcHandler targetProcess access inherit opts = alloca $ \res -> do
    failIfFalse_ "duplicateHandle: DuplicateHandle" $ c_DuplicateHandle srcProccess srcHandler targetProcess res (fromIntegral access) inherit opts
    peek res

setEvent :: HANDLE -> IO ()
setEvent event = failIfFalse_ "setEvent: SetEvent" $ c_SetEvent event

resetEvent :: HANDLE -> IO ()
resetEvent event = failIfFalse_ "resetEvent: ResetEvent" $ c_ResetEvent event

pulseEvent :: HANDLE -> IO ()
pulseEvent event = failIfFalse_ "pulseEvent: PulseEvent" $ c_PulseEvent event

signalObjectAndWait :: HANDLE -> HANDLE -> MilliSeconds -> Bool -> IO WaitResult
signalObjectAndWait toSignal toWaitOn millis alterable = c_SignalObjectAndWait toSignal toWaitOn millis alterable

waitForSingleObject :: HANDLE -> MilliSeconds -> IO WaitResult
waitForSingleObject toWaitOn millis = c_WaitForSingleObject toWaitOn millis

waitForSingleObjectEx :: HANDLE -> MilliSeconds -> Bool -> IO WaitResult
waitForSingleObjectEx toWaitOn millis alterable = c_WaitForSingleObjectEx toWaitOn millis alterable

waitForMultipleObjects :: [HANDLE] -> Bool -> MilliSeconds -> IO WaitResult
waitForMultipleObjects hs waitAll millis = withArrayLen hs $ \n hsp ->
  c_WaitForMultipleObjects (fromIntegral n) hsp waitAll millis

waitForMultipleObjectsEx :: [HANDLE] -> Bool -> MilliSeconds -> Bool -> IO WaitResult
waitForMultipleObjectsEx hs waitAll millis alterable = withArrayLen hs $ \n hsp ->
  c_WaitForMultipleObjectsEx (fromIntegral n) hsp waitAll millis alterable

---------------------------------------------------------------------------
-- Imports
---------------------------------------------------------------------------
foreign import WINDOWS_CCONV "windows.h OpenEventW"
    c_OpenEvent :: DWORD -> BOOL -> LPCTSTR -> IO HANDLE

foreign import WINDOWS_CCONV "windows.h CreateEventW"
    c_CreateEvent :: LPSECURITY_ATTRIBUTES -> BOOL -> BOOL -> LPCTSTR -> IO HANDLE

foreign import WINDOWS_CCONV "windows.h DuplicateHandle"
    c_DuplicateHandle :: HANDLE -> HANDLE -> HANDLE -> Ptr HANDLE -> DWORD -> BOOL -> DWORD -> IO BOOL

foreign import WINDOWS_CCONV "windows.h SetEvent"
    c_SetEvent :: HANDLE -> IO Bool

foreign import WINDOWS_CCONV "windows.h ResetEvent"
    c_ResetEvent :: HANDLE -> IO Bool

foreign import WINDOWS_CCONV "windows.h PulseEvent"
    c_PulseEvent :: HANDLE -> IO Bool

foreign import WINDOWS_CCONV "windows.h SignalObjectAndWait"
    c_SignalObjectAndWait :: HANDLE -> HANDLE -> DWORD -> BOOL -> IO DWORD

foreign import WINDOWS_CCONV "windows.h WaitForSingleObject"
    c_WaitForSingleObject :: HANDLE -> DWORD -> IO DWORD

foreign import WINDOWS_CCONV "windows.h WaitForSingleObjectEx"
    c_WaitForSingleObjectEx :: HANDLE -> DWORD -> BOOL -> IO DWORD

foreign import WINDOWS_CCONV "windows.h WaitForMultipleObjects"
    c_WaitForMultipleObjects :: DWORD -> Ptr HANDLE -> BOOL -> DWORD -> IO DWORD

foreign import WINDOWS_CCONV "windows.h WaitForMultipleObjectsEx"
    c_WaitForMultipleObjectsEx :: DWORD -> Ptr HANDLE -> BOOL -> DWORD -> BOOL -> IO DWORD
