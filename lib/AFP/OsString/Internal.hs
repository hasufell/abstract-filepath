{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnliftedFFITypes #-}

module AFP.OsString.Internal where

import AFP.Data.ByteString.Short.Decode
    ( decodeUtf16LE
    , decodeUtf16LE'
    , decodeUtf16LE''
    , decodeUtf16LEWith
    , decodeUtf8
    , decodeUtf8'
    , decodeUtf8With
    )
import AFP.Data.ByteString.Short.Encode
    ( encodeUtf16LE, encodeUtf8 )
import AFP.OsString.Internal.Types

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
import Data.Word8
    ( _asterisk
    , _backslash
    , _bar
    , _colon
    , _greater
    , _less
    , _nul
    , _question
    , _quotedbl
    , _slash
    )
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

import AFP.Data.ByteString.Short
    ( toShort )
import qualified AFP.Data.ByteString.Short as BSP
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import qualified AFP.Data.ByteString.Short.Word16 as BS
#else
import qualified AFP.Data.ByteString.Short as BS
#endif
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified GHC.Foreign as GHC
import qualified Language.Haskell.TH.Syntax as TH
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import AFP.OsString.Windows hiding ( fromChar )
import qualified AFP.OsString.Windows as PF
#else
import AFP.OsString.Posix  hiding ( fromChar )
import qualified AFP.OsString.Posix as PF
#endif




-- | Total Unicode-friendly encoding.
--
-- On windows this encodes as UTF16, which is expected.
-- On unix this encodes as UTF8, which is a good guess.
toOsString :: String -> OsString
toOsString = OsString . toPlatformString

-- | Like 'toOsString', except on unix this uses the current
-- locale for encoding instead of always UTF8.
--
-- Looking up the locale requires IO. If you're not worried about calls
-- to 'setFileSystemEncoding', then 'unsafePerformIO' may be feasible.
toOsStringIO :: String -> IO OsString
toOsStringIO = fmap OsString . toPlatformStringIO


-- | Partial unicode friendly decoding.
--
-- On windows this decodes as UTF16 (which is the expected filename encoding).
-- On unix this decodes as UTF8 (which is a good guess). Note that
-- filenames on unix are encoding agnostic char arrays.
--
-- Throws a 'UnicodeException' if decoding fails.
fromOsString :: MonadThrow m => OsString -> m String
fromOsString (OsString x) = fromPlatformString x


-- | Like 'fromOsString', except on unix this uses the current
-- locale for decoding instead of always UTF8.
--
-- Looking up the locale requires IO. If you're not worried about calls
-- to 'setFileSystemEncoding', then 'unsafePerformIO' may be feasible.
--
-- Throws 'UnicodeException' if decoding fails.
fromOsStringIO :: OsString -> IO String
fromOsStringIO (OsString x) = fromPlatformStringIO x


-- | Constructs an @OsString@ from a ByteString.
--
-- On windows, this ensures valid UTF16, on unix it is passed unchanged/unchecked.
--
-- Throws 'UnicodeException' on invalid UTF16 on windows.
bsToOsString :: MonadThrow m
             => ByteString
             -> m OsString
bsToOsString = fmap OsString . bsToPlatformString


qq :: (ByteString -> Q Exp) -> QuasiQuoter
qq quoteExp' =
  QuasiQuoter
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
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

mkOsString :: ByteString -> Q Exp
mkOsString bs = 
  case bsToOsString bs of
    Just afp -> lift afp
    Nothing -> error "invalid encoding"

-- | QuasiQuote an 'OsString'. This accepts Unicode characters
-- and encodes as UTF-8 on unix and UTF-16 on windows.
osstr :: QuasiQuoter
osstr = qq mkOsString


unpackOsString :: OsString -> [OsWord]
unpackOsString (OsString x) = fmap OsWord $ unpackPlatformString x


packOsString :: [OsWord] -> OsString
packOsString = OsString . packPlatformString . fmap (\(OsWord x) -> x)


fromChar :: Char -> OsWord
fromChar = OsWord . PF.fromChar
