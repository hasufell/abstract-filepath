{-# LANGUAGE CPP #-}
-- This template expects CPP definitions for:
--     MODULE_NAME = Posix | Windows
--     IS_WINDOWS  = False | True

module AFP.OsString.MODULE_NAME
  (
  -- * Types
#ifdef WINDOWS
    WindowsString
  , WindowsWord
#else
    PosixString
  , PosixWord
#endif

  -- * String construction
  , toPlatformString
  , toPlatformStringIO
  , bsToPlatformString
  , pstr
  , packPlatformString

  -- * String deconstruction
  , fromPlatformString
  , fromPlatformStringIO
  , unpackPlatformString

  -- * Word construction
  , fromChar

  )
where


import AFP.OsString.Internal.Types
#ifdef WINDOWS
  ( WindowsString
  , WindowsWord
  )
#else
  ( PosixString
  , PosixWord
  )
#endif
import AFP.Data.ByteString.Short.Encode
    ( encodeUtf16LE, encodeUtf8 )
import AFP.Data.ByteString.Short.Decode
    ( decodeUtf16LE
    , decodeUtf16LE'
    , decodeUtf16LE''
    , decodeUtf16LEWith
    , decodeUtf8
    , decodeUtf8'
    , decodeUtf8With
    )
import AFP.AbstractFilePath.Internal.Types
import AFP.OsString.Internal.Types

import Control.Arrow
    ( second )
import Data.Bifunctor
    ( bimap, first )
import Data.ByteString
    ( ByteString )
import Data.ByteString.Short
    ( ShortByteString )
import Data.Maybe
    ( isJust )
import Data.Word8
    ( Word8, _colon, _nul, _period, _slash, _underscore )
import Control.Exception
    ( throwIO )
import Control.Monad.Catch
    ( MonadThrow, throwM )
import Data.ByteString
    ( ByteString )
import Data.Proxy
    ( Proxy (..) )
import Data.Text.Encoding.Error
    ( UnicodeException (..), lenientDecode )
import Data.Typeable
import GHC.Exts
    ( IsString (..) )
import GHC.IO.Encoding
    ( getFileSystemEncoding )
import GHC.IO.Exception
    ( IOErrorType (InvalidArgument) )
import Language.Haskell.TH
import Language.Haskell.TH.Quote
    ( QuasiQuoter (..) )
import Language.Haskell.TH.Syntax
    ( Lift (..), lift )
import System.IO.Error
    ( catchIOError )

import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified GHC.Foreign as GHC
import qualified Language.Haskell.TH.Syntax as TH

import qualified AFP.AbstractFilePath.Internal.MODULE_NAME as C
#ifdef WINDOWS
import qualified AFP.Data.ByteString.Short.Word16 as BS
#else
import qualified AFP.Data.ByteString.Short as BS
#endif
import AFP.Data.ByteString.Short (toShort)



-- | Total Unicode-friendly encoding.
--
-- On windows this encodes as UTF16, which is expected.
-- On unix this encodes as UTF8, which is a good guess.
toPlatformString :: String -> PLATFORM_STRING
#ifdef WINDOWS
toPlatformString = WS . encodeUtf16LE
#else
toPlatformString = PS . encodeUtf8
#endif

-- | Like 'toPlatformString', except on unix this uses the current
-- locale for encoding instead of always UTF8.
--
-- Looking up the locale requires IO. If you're not worried about calls
-- to 'setFileSystemEncoding', then 'unsafePerformIO' may be feasible.
toPlatformStringIO :: String -> IO PLATFORM_STRING
#ifdef WINDOWS
toPlatformStringIO = pure . WS . encodeUtf16LE
#else
toPlatformStringIO str = do
  enc <- getFileSystemEncoding
  cstr <- GHC.newCString enc str
  PS <$> BS.packCString cstr
#endif


-- | Partial unicode friendly decoding.
--
-- On windows this decodes as UTF16 (which is the expected filename encoding).
-- On unix this decodes as UTF8 (which is a good guess). Note that
-- filenames on unix are encoding agnostic char arrays.
--
-- Throws a 'UnicodeException' if decoding fails.
fromPlatformString :: MonadThrow m => PLATFORM_STRING -> m String
#ifdef WINDOWS
fromPlatformString (WS ba) = either throwM pure $ decodeUtf16LE' ba
#else
fromPlatformString (PS ba) = either throwM pure $ decodeUtf8' ba
#endif


-- | Like 'fromPlatformStringIO', except on unix this uses the current
-- locale for decoding instead of always UTF8.
--
-- Looking up the locale requires IO. If you're not worried about calls
-- to 'setFileSystemEncoding', then 'unsafePerformIO' may be feasible.
--
-- Throws 'UnicodeException' if decoding fails.
fromPlatformStringIO :: PLATFORM_STRING -> IO String
#ifdef WINDOWS
fromPlatformStringIO (WS ba) = either throwIO pure $ decodeUtf16LE' ba
#else
fromPlatformStringIO (PS ba) = flip catchIOError (\_ -> throwIO (DecodeError "fromAbstractFilePath' failed" Nothing))
  $ BS.useAsCString ba $ \fp -> getFileSystemEncoding >>= \enc -> GHC.peekCString enc fp
#endif


-- | Constructs an platform string from a ByteString.
--
-- On windows, this ensures valid UTF16, on unix it is passed unchanged/unchecked.
--
-- Throws 'UnicodeException' on invalid UTF16 on windows.
bsToPlatformString :: MonadThrow m
             => ByteString
             -> m PLATFORM_STRING
#ifdef WINDOWS
bsToPlatformString bs =
  either throwM (const . pure . WS . toShort $ bs) $ decodeUtf16LE'' bs
#else
bsToPlatformString = pure . PS . toShort
#endif


qq :: (ByteString -> Q Exp) -> QuasiQuoter
qq quoteExp' =
  QuasiQuoter
#ifdef WINDOWS
  { quoteExp  = (\s -> quoteExp' . E.encodeUtf16LE . T.pack $ s)
  , quotePat  = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a pattern)"
  , quoteType = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a type)"
  , quoteDec  = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a declaration)"
  }
#else
  { quoteExp  = (\s -> quoteExp' . E.encodeUtf8 . T.pack $ s)
  , quotePat  = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a pattern)"
  , quoteType = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a type)"
  , quoteDec  = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a declaration)"
  }
#endif

mkPlatformString :: ByteString -> Q Exp
mkPlatformString bs = 
  case bsToPlatformString bs of
    Just afp -> lift afp
    Nothing -> error "invalid encoding"

-- | QuasiQuote a 'PLATFORM_STRING'. This accepts Unicode characters
-- and encodes as UTF-8 on unix and UTF-16 on windows.
pstr :: QuasiQuoter
pstr = qq mkPlatformString


unpackPlatformString :: PLATFORM_STRING -> [PLATFORM_WORD]
#ifdef WINDOWS
unpackPlatformString (WS ba) = fmap WW $ BS.unpack ba
#else
unpackPlatformString (PS ba) = fmap PW $ BS.unpack ba
#endif


packPlatformString :: [PLATFORM_WORD] -> PLATFORM_STRING
#ifdef WINDOWS
packPlatformString ws = WS . BS.pack . fmap (\(WW w) -> w) $ ws
#else
packPlatformString ws = PS . BS.pack . fmap (\(PW w) -> w) $ ws
#endif


fromChar :: Char -> PLATFORM_WORD
#ifdef WINDOWS
fromChar = WW . fromIntegral . fromEnum
#else
fromChar = PW . fromIntegral . fromEnum
#endif

