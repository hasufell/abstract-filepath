{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module AFP.OsString.Internal.Types
  (
    WindowsString(..)
  , PosixString(..)
  , PlatformString
  , WindowsChar(..)
  , PosixChar(..)
  , PlatformChar
  , OsString(..)
  , OsChar(..)
  )
where


import Data.Word
import Data.Bits
    ( (.&.) )
import "bytestring" Data.ByteString.Short
    ( ShortByteString )
import Data.Text.Internal.Unsafe.Shift
    ( shiftL, shiftR )
import GHC.Exts
    ( IsString (..) )
import Language.Haskell.TH.Syntax
    ( Lift (..), lift )
import Data.Text.Encoding.Error
    ( OnDecodeError, strictDecode )
import Data.Text.Internal.Fusion.Size
    ( maxSize )
import Data.Text.Internal.Fusion.Types
    ( Step (..), Stream (..) )
import Data.Text.Internal.Unsafe.Char
    ( ord, unsafeChr, unsafeChr8 )
import qualified Data.Text.Internal.Encoding.Utf16 as U16
import qualified Data.Text.Internal.Encoding.Utf8 as U8
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#endif


import qualified Data.ByteString.Short as BS
#if MIN_VERSION_template_haskell(2,16,0)
import qualified Language.Haskell.TH.Syntax as TH
#endif

-- Using unpinned bytearrays to avoid Heap fragmentation and
-- which are reasonably cheap to pass to FFI calls
-- wrapped with typeclass-friendly types allowing to avoid CPP
-- 
-- Note that, while unpinned bytearrays incur a memcpy on each
-- FFI call, this overhead is generally much preferable to
-- the memory fragmentation of pinned bytearrays

-- | Commonly used windows string as UTF16 bytes.
newtype WindowsString = WS { unWFP :: BS.ShortByteString }
  deriving (Eq, Ord, Semigroup, Monoid)

instance Lift WindowsString where
  lift (WS bs)
    = [| (WS (BS.pack $(lift $ BS.unpack bs))) :: WindowsString |]
#if MIN_VERSION_template_haskell(2,17,0)
  liftTyped = TH.unsafeCodeCoerce . TH.lift
#elif MIN_VERSION_template_haskell(2,16,0)
  liftTyped = TH.unsafeTExpCoerce . TH.lift
#endif

-- | Commonly used Posix string as uninterpreted @char[]@
-- array.
newtype PosixString   = PS { unPFP :: BS.ShortByteString }
  deriving (Eq, Ord, Semigroup, Monoid)

instance Lift PosixString where
  lift (PS bs)
    = [| (PS (BS.pack $(lift $ BS.unpack bs))) :: PosixString |]
#if MIN_VERSION_template_haskell(2,17,0)
  liftTyped = TH.unsafeCodeCoerce . TH.lift
#elif MIN_VERSION_template_haskell(2,16,0)
  liftTyped = TH.unsafeTExpCoerce . TH.lift
#endif

instance Show WindowsString where
  show (WS bs) = ('\"': decodeUtf16LE bs) <> "\""

instance Show PosixString where
  show (PS bs) = ('\"': decodeUtf8 bs) <> "\""

instance IsString WindowsString where 
    fromString = WS . encodeUtf16LE

instance IsString PosixString where 
    fromString = PS . encodeUtf8

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
type PlatformString = WindowsString
#else
type PlatformString = PosixString
#endif

newtype WindowsChar = WW Word16
  deriving (Eq, Ord, Show)
newtype PosixChar   = PW Word8
  deriving (Eq, Ord, Show)

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
type PlatformChar = WindowsChar
#else
type PlatformChar = PosixChar
#endif


-- | Newtype representing short operating system specific strings.
--
-- Internally this is either 'WindowsString or 'PosixString,
-- depending on the platform. Both use unpinned
-- 'ShortByteString' for efficiency.
--
-- The constructor is only exported via "AFP.OsString.Internal.Types", since
-- dealing with the internals isn't generally recommended, but supported
-- in case you need to write platform specific code.
newtype OsString = OsString PlatformString

-- | Byte equality of the internal representation.
instance Eq OsString where
  (OsString a) == (OsString b) = a == b

-- | Byte ordering of the internal representation.
instance Ord OsString where
  compare (OsString a) (OsString b) = compare a b

-- | Encodes as UTF16 on windows and UTF8 on unix.
instance IsString OsString where 
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
    fromString = OsString . WS . encodeUtf16LE
#else
    fromString = OsString . PS . encodeUtf8
#endif


-- | \"String-Concatenation\" for 'AbstractFilePath'. This is __not__ the same
-- as '(</>)'.
instance Monoid OsString where 
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
    mempty      = OsString (WS BS.empty)
    mappend (OsString (WS a)) (OsString (WS b))
      = OsString (WS (mappend a b))
#else
    mempty      = OsString (PS BS.empty)
    mappend (OsString (PS a)) (OsString (PS b))
      = OsString (PS (mappend a b))
#endif
#if MIN_VERSION_base(4,11,0)
instance Semigroup OsString where 
    (<>) = mappend
#endif


instance Lift OsString where
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
  lift (OsString (WS bs))
    = [| OsString (WS (BS.pack $(lift $ BS.unpack bs))) :: OsString |]
#else
  lift (OsString (PS bs))
    = [| OsString (PS (BS.pack $(lift $ BS.unpack bs))) :: OsString |]
#endif
#if MIN_VERSION_template_haskell(2,17,0)
  liftTyped = TH.unsafeCodeCoerce . TH.lift
#elif MIN_VERSION_template_haskell(2,16,0)
  liftTyped = TH.unsafeTExpCoerce . TH.lift
#endif

instance Show OsString where
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
  show (OsString (WS bs)) = ('\"': decodeUtf16LE bs) <> "\""
#else
  show (OsString (PS bs)) = ('\"': decodeUtf8 bs) <> "\""
#endif


-- | Newtype representing operating system Word with respect to
-- the encoding. On Windows, this is 'Word16', on POSIX 'Word8'.
newtype OsChar = OsChar PlatformChar
  deriving Show

-- | Byte equality of the internal representation.
instance Eq OsChar where
  (OsChar a) == (OsChar b) = a == b

-- | Byte ordering of the internal representation.
instance Ord OsChar where
  compare (OsChar a) (OsChar b) = compare a b



-------------------------------------------------------
-- Inlined definitions to avoid dependency


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



encodeUtf8 :: String -> ShortByteString
encodeUtf8 = BS.pack . encode
  where
    encode :: String -> [Word8]
    encode = concatMap encodeChar

    encodeChar :: Char -> [Word8]
    encodeChar = map fromIntegral . go . ord
     where
      go oc
       | oc <= 0x7f       = [oc]

       | oc <= 0x7ff      = [ 0xc0 + (oc `shiftR` 6)
                            , 0x80 + oc .&. 0x3f
                            ]

       | oc <= 0xffff     = [ 0xe0 + (oc `shiftR` 12)
                            , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                            , 0x80 + oc .&. 0x3f
                            ]
       | otherwise        = [ 0xf0 + (oc `shiftR` 18)
                            , 0x80 + ((oc `shiftR` 12) .&. 0x3f)
                            , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                            , 0x80 + oc .&. 0x3f
                            ]
{-# INLINE encodeUtf8 #-}


encodeUtf16LE :: String -> ShortByteString
encodeUtf16LE = BS.pack . encode
  where
    encode :: String -> [Word8]
    encode = concatMap encodeChar

    encodeChar :: Char -> [Word8]
    encodeChar = map fromIntegral . go . ord
      where
        go oc
          | oc < 0x10000 = [ oc, oc `shiftR` 8 ]
          | otherwise =
            let m = oc - 0x10000
            in [ m `shiftR` 10
               , (m `shiftR` 18) + 0xD8
               , m .&. 0x3FF 
               , ((m .&. 0x3FF) `shiftR` 8) + 0xDC ]
{-# INLINE encodeUtf16LE #-}

