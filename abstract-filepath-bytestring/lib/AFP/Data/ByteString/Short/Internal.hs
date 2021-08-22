{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnboxedTuples #-}

module AFP.Data.ByteString.Short.Internal
  ( create
  , asBA
  , BA(..)
  , MBA(..)
  , newPinnedByteArray
  , newByteArray
  , copyByteArray
  , unsafeFreezeByteArray
  , useAsCString
  , useAsCStringLen
  , useAsCWString
  , useAsCWStringLen
  , packCString
  , packCStringLen
  , packCWString
  , packCWStringLen
  , newCWString
  )

where

import Prelude hiding
    ( length )
import GHC.Exts
import GHC.ST
    ( ST (ST), runST )
import Data.Word
import Foreign.C.String hiding (newCWString)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Array (mallocArray0)
import Foreign.Storable (pokeByteOff)
import Data.ByteString.Short.Internal
import Control.Exception ( throwIO )
#if MIN_VERSION_bytestring(0,10,9)
import Data.ByteString.Internal (c_strlen)
#else
import Data.Word
import Foreign.C.Types
#endif


create :: Int -> (forall s. MBA s -> ST s ()) -> ShortByteString
create len fill =
    runST $ do
      mba <- newByteArray len
      fill mba
      BA# ba# <- unsafeFreezeByteArray mba
      return (SBS ba#)
{-# INLINE create #-}


asBA :: ShortByteString -> BA
asBA (SBS ba#) = BA# ba#



data BA    = BA# ByteArray#
data MBA s = MBA# (MutableByteArray# s)


newPinnedByteArray :: Int -> ST s (MBA s)
newPinnedByteArray (I# len#) =
    ST $ \s -> case newPinnedByteArray# len# s of
                 (# s', mba# #) -> (# s', MBA# mba# #)

newByteArray :: Int -> ST s (MBA s)
newByteArray (I# len#) =
    ST $ \s -> case newByteArray# len# s of
                 (# s', mba# #) -> (# s', MBA# mba# #)

copyByteArray :: BA -> Int -> MBA s -> Int -> Int -> ST s ()
copyByteArray (BA# src#) (I# src_off#) (MBA# dst#) (I# dst_off#) (I# len#) =
    ST $ \s -> case copyByteArray# src# src_off# dst# dst_off# len# s of
                 s' -> (# s', () #)

unsafeFreezeByteArray :: MBA s -> ST s BA
unsafeFreezeByteArray (MBA# mba#) =
    ST $ \s -> case unsafeFreezeByteArray# mba# s of
                 (# s', ba# #) -> (# s', BA# ba# #)


-- this is a copy-paste from bytestring
#if !MIN_VERSION_bytestring(0,10,9)
------------------------------------------------------------------------
-- Primop replacements

-- ---------------------------------------------------------------------
--
-- Standard C functions
--

foreign import ccall unsafe "string.h strlen" c_strlen
    :: CString -> IO CSize


-- ---------------------------------------------------------------------
--
-- Uses our C code
--

-- | /O(n)./ Construct a new @ShortByteString@ from a @CString@. The
-- resulting @ShortByteString@ is an immutable copy of the original
-- @CString@, and is managed on the Haskell heap. The original
-- @CString@ must be null terminated.
--
-- @since 0.10.10.0
packCString :: CString -> IO ShortByteString
packCString cstr = do
  len <- c_strlen cstr
  packCStringLen (cstr, fromIntegral len)

-- | /O(n)./ Construct a new @ShortByteString@ from a @CStringLen@. The
-- resulting @ShortByteString@ is an immutable copy of the original @CStringLen@.
-- The @ShortByteString@ is a normal Haskell value and will be managed on the
-- Haskell heap.
--
-- @since 0.10.10.0
packCStringLen :: CStringLen -> IO ShortByteString
packCStringLen (cstr, len) | len >= 0 = createFromPtr cstr len
packCStringLen (_, len) =
  moduleErrorIO "packCStringLen" ("negative length: " ++ show len)

-- | /O(n) construction./ Use a @ShortByteString@ with a function requiring a
-- null-terminated @CString@.  The @CString@ is a copy and will be freed
-- automatically; it must not be stored or used after the
-- subcomputation finishes.
--
-- @since 0.10.10.0
useAsCString :: ShortByteString -> (CString -> IO a) -> IO a
useAsCString bs action =
  allocaBytes (l+1) $ \buf -> do
      copyToPtr bs 0 buf (fromIntegral l)
      pokeByteOff buf l (0::Word8)
      action buf
  where l = length bs

-- | /O(n) construction./ Use a @ShortByteString@ with a function requiring a @CStringLen@.
-- As for @useAsCString@ this function makes a copy of the original @ShortByteString@.
-- It must not be stored or used after the subcomputation finishes.
--
-- @since 0.10.10.0
useAsCStringLen :: ShortByteString -> (CStringLen -> IO a) -> IO a
useAsCStringLen bs action =
  allocaBytes l $ \buf -> do
      copyToPtr bs 0 buf (fromIntegral l)
      action (buf, l)
  where l = length bs


#endif


-- | /O(n)./ Construct a new @ShortByteString@ from a @CWString@. The
-- resulting @ShortByteString@ is an immutable copy of the original
-- @CWString@, and is managed on the Haskell heap. The original
-- @CWString@ must be null terminated.
--
-- @since 0.10.10.0
packCWString :: CWString -> IO ShortByteString
packCWString cstr = do
  len <- c_strlen (coerce cstr)
  packCWStringLen (cstr, fromIntegral len)

-- | /O(n)./ Construct a new @ShortByteString@ from a @CWStringLen@. The
-- resulting @ShortByteString@ is an immutable copy of the original @CWStringLen@.
-- The @ShortByteString@ is a normal Haskell value and will be managed on the
-- Haskell heap.
--
-- @since 0.10.10.0
packCWStringLen :: CWStringLen -> IO ShortByteString
packCWStringLen (cstr, len) | len >= 0 = createFromPtr cstr len
packCWStringLen (_, len) =
  moduleErrorIO "packCWStringLen" ("negative length: " ++ show len)


-- | /O(n) construction./ Use a @ShortByteString@ with a function requiring a
-- null-terminated @CWString@.  The @CWString@ is a copy and will be freed
-- automatically; it must not be stored or used after the
-- subcomputation finishes.
--
-- @since 0.10.10.0
useAsCWString :: ShortByteString -> (CWString -> IO a) -> IO a
useAsCWString bs action =
  allocaBytes (l+1) $ \buf -> do
      copyToPtr bs 0 buf (fromIntegral l)
      pokeByteOff buf l (0::Word8)
      action buf
  where l = length bs

-- | /O(n) construction./ Use a @ShortByteString@ with a function requiring a @CWStringLen@.
-- As for @useAsCWString@ this function makes a copy of the original @ShortByteString@.
-- It must not be stored or used after the subcomputation finishes.
--
-- @since 0.10.10.0
useAsCWStringLen :: ShortByteString -> (CWStringLen -> IO a) -> IO a
useAsCWStringLen bs action =
  allocaBytes l $ \buf -> do
      copyToPtr bs 0 buf (fromIntegral l)
      action (buf, l)
  where l = length bs

-- | /O(n) construction./ Use a @ShortByteString@ with a function requiring a @CWStringLen@.
-- As for @useAsCWString@ this function makes a copy of the original @ShortByteString@.
-- It must not be stored or used after the subcomputation finishes.
--
-- @since 0.10.10.0
newCWString :: ShortByteString -> IO CWString
newCWString bs = do
  ptr <- mallocArray0 l
  copyToPtr bs 0 ptr (fromIntegral l)
  pokeByteOff ptr l (0::Word8)
  return ptr
  where l = length bs


 -- ---------------------------------------------------------------------
-- Internal utilities

moduleErrorIO :: String -> String -> IO a
moduleErrorIO fun msg = throwIO . userError $ moduleErrorMsg fun msg
{-# NOINLINE moduleErrorIO #-}

moduleErrorMsg :: String -> String -> String
moduleErrorMsg fun msg = "Data.ByteString.Short." ++ fun ++ ':':' ':msg
