{-# LANGUAGE RankNTypes #-}

module AbstractFilePath.Internal.Decode (decodeUtf16LE, decodeUtf16LEWith, decodeUtf8, decodeUtf8With) where

import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString)
import Data.Text (Text)
import Data.Text.Encoding.Error
import Data.Text.Internal.Fusion (Stream(..), Step)
import Data.Text.Internal.Fusion.Size
import Data.Text.Internal.Fusion.Types
import Data.Text.Internal.Private (runText)
import Data.Text.Internal.Unsafe.Char (ord, unsafeChr, unsafeChr8, unsafeWrite)
import Data.Text.Internal.Unsafe.Shift (shiftL, shiftR)
import Data.Word

import qualified Data.ByteString       as B
import qualified Data.ByteString.Short as BS (ShortByteString(..), toShort, fromShort, length, index)
import qualified Data.ByteString.Unsafe as B
import qualified Data.Text as T
import qualified Data.Text.Array as A
import qualified Data.Text.Encoding as E
import qualified Data.Text.Internal as I
import qualified Data.Text.Internal.Encoding.Utf8 as U8
import qualified Data.Text.Internal.Encoding.Utf16 as U16
import qualified Data.Text.Internal.Fusion.Common as S


-- | /O(n)/ Convert a 'ByteString' into a 'Stream Char', using UTF-8
-- encoding.
streamUtf8 :: OnDecodeError -> ShortByteString -> Stream Char
streamUtf8 onErr bs = Stream next 0 (maxSize l)
    where
      l = BS.length bs
      next i
          | i >= l = Done
          | U8.validate1 x1 = Yield (unsafeChr8 x1) (i+1)
          | i+1 < l && U8.validate2 x1 x2 = Yield (U8.chr2 x1 x2) (i+2)
          | i+2 < l && U8.validate3 x1 x2 x3 = Yield (U8.chr3 x1 x2 x3) (i+3)
          | i+3 < l && U8.validate4 x1 x2 x3 x4 = Yield (U8.chr4 x1 x2 x3 x4) (i+4)
          | otherwise = decodeError "streamUtf8" "UTF-8" onErr (Just x1) (i+1)
          where
            x1 = idx i
            x2 = idx (i + 1)
            x3 = idx (i + 2)
            x4 = idx (i + 3)
            idx = BS.index bs
{-# INLINE [0] streamUtf8 #-}

-- | /O(n)/ Convert a 'ShortByteString' into a 'Stream Char', using little
-- endian UTF-16 encoding.
streamUtf16LE :: OnDecodeError -> ShortByteString -> Stream Char
streamUtf16LE onErr bs = Stream next 0 (maxSize (l `shiftR` 1))
    where
      l = BS.length bs
      {-# INLINE next #-}
      next i
          | i >= l                         = Done
          | i+1 < l && U16.validate1 x1    = Yield (unsafeChr x1) (i+2)
          | i+3 < l && U16.validate2 x1 x2 = Yield (U16.chr2 x1 x2) (i+4)
          | otherwise = decodeError "streamUtf16LE" "UTF-16LE" onErr Nothing (i+1)
          where
            x1    = idx i       + (idx (i + 1) `shiftL` 8)
            x2    = idx (i + 2) + (idx (i + 3) `shiftL` 8)
            idx = fromIntegral . BS.index bs :: Int -> Word16
{-# INLINE [0] streamUtf16LE #-}

-- | Decode text from little endian UTF-16 encoding.
decodeUtf16LEWith :: OnDecodeError -> ShortByteString -> String
decodeUtf16LEWith onErr bs = unstream (streamUtf16LE onErr bs)
{-# INLINE decodeUtf16LEWith #-}

-- | Decode text from little endian UTF-16 encoding.
--
-- If the input contains any invalid little endian UTF-16 data, an
-- exception will be thrown.  For more control over the handling of
-- invalid data, use 'decodeUtf16LEWith'.
decodeUtf16LE :: ShortByteString -> String
decodeUtf16LE = decodeUtf16LEWith strictDecode
{-# INLINE decodeUtf16LE #-}

-- | Decode text from little endian UTF-16 encoding.
decodeUtf8With :: OnDecodeError -> ShortByteString -> String
decodeUtf8With onErr bs = unstream (streamUtf8 onErr bs)
{-# INLINE decodeUtf8With #-}

-- | Decode text from little endian UTF-16 encoding.
--
-- If the input contains any invalid little endian UTF-16 data, an
-- exception will be thrown.  For more control over the handling of
-- invalid data, use 'decodeUtf16LEWith'.
decodeUtf8 :: ShortByteString -> String
decodeUtf8 = decodeUtf8With strictDecode
{-# INLINE decodeUtf8 #-}


decodeError :: forall s. String -> String -> OnDecodeError -> Maybe Word8
            -> s -> Step s Char
decodeError func kind onErr mb i =
    case onErr desc mb of
      Nothing -> Skip i
      Just c  -> Yield c i
    where desc = "Data.Text.Internal.Encoding.Fusion." ++ func ++ ": Invalid " ++
                 kind ++ " stream"

-- | /O(n)/ Convert a 'Stream Char' into a 'Text'.
unstream :: Stream Char -> String
unstream (Stream next0 s0 _) = go s0
  where
    go si =
      case next0 si of
          Done        -> []
          Skip si'    -> go si'
          Yield c si' -> c : go si'
{-# INLINE [0] unstream #-}
