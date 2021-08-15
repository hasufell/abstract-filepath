{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnliftedFFITypes #-}

module AbstractFilePath.Internal where

import AbstractFilePath.Internal.Types
import OsString.Internal hiding
    ( fromByteString, qq )
import qualified OsString.Internal as OS

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

import qualified Data.ByteString.Short as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified GHC.Foreign as GHC
import qualified Language.Haskell.TH.Syntax as TH


-- | Total Unicode-friendly encoding.
--
-- On windows this encodes as UTF16, which is expected.
-- On unix this encodes as UTF8, which is a good guess.
toAbstractFilePath :: String -> AbstractFilePath
toAbstractFilePath = toOsString


-- | Like 'toAbstractFilePath', except on unix this uses the current
-- locale for encoding instead of always UTF8.
--
-- Looking up the locale requires IO. If you're not worried about calls
-- to 'setFileSystemEncoding', then 'unsafePerformIO' may be feasible.
toAbstractFilePath' :: String -> IO AbstractFilePath
toAbstractFilePath' = toOsString'


-- | Partial unicode friendly decoding.
--
-- On windows this decodes as UTF16 (which is the expected filename encoding).
-- On unix this decodes as UTF8 (which is a good guess). Note that
-- filenames on unix are encoding agnostic char arrays.
--
-- Throws a 'UnicodeException' if decoding fails.
--
-- Note that filenames of different encodings may have the same @String@
-- representation, although they're not the same byte-wise.
fromAbstractFilePath :: MonadThrow m => AbstractFilePath -> m String
fromAbstractFilePath = fromOsString


-- | Like 'fromAbstractFilePath', except on unix this uses the current
-- locale for decoding instead of always UTF8.
--
-- Looking up the locale requires IO. If you're not worried about calls
-- to 'setFileSystemEncoding', then 'unsafePerformIO' may be feasible.
--
-- Throws 'UnicodeException' if decoding fails.
fromAbstractFilePath' :: AbstractFilePath -> IO String
fromAbstractFilePath' = fromOsString'


-- | Constructs an @AbstractFilePath@ from a ByteString.
--
-- On windows, this ensures valid UTF16, on unix it is passed unchanged/unchecked.
--
-- Throws 'UnicodeException' on invalid UTF16 on windows.
fromByteString :: MonadThrow m
               => ByteString
               -> m AbstractFilePath
fromByteString = OS.fromByteString


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

mkAbstractFilePath :: ByteString -> Q Exp
mkAbstractFilePath bs = 
  case fromByteString bs of
    Just afp ->
      if True -- isValid afp -- TODO
      then lift afp
      else error "invalid filepath"
    Nothing -> error "invalid encoding"

-- | QuasiQuote an 'AbstractFilePath'. This accepts Unicode characters
-- and encodes as UTF-8 on unix and UTF-16 on windows. Runs 'filepathIsValid'
-- on the input.
absfp :: QuasiQuoter
absfp = qq mkAbstractFilePath

