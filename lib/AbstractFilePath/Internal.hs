{-# LANGUAGE CPP, RankNTypes, UnliftedFFITypes, TemplateHaskell #-}

module AbstractFilePath.Internal where

import AbstractFilePath.Internal.Types
import AbstractFilePath.Internal.Decode (decodeUtf16LE, decodeUtf16LEWith, decodeUtf16LE', decodeUtf16LE'', decodeUtf8, decodeUtf8With, decodeUtf8')
import AbstractFilePath.Internal.Encode (encodeUtf16LE, encodeUtf8)

import Control.Exception (throwIO)
import Control.Monad.Catch (MonadThrow, throwM)
import Data.ByteString ( ByteString )
import Data.Text.Encoding.Error (lenientDecode, UnicodeException(..))
import Data.Word8 (_nul, _less, _greater, _colon, _quotedbl, _slash, _backslash, _bar, _question, _asterisk)
import Data.Proxy ( Proxy (..) )
import Data.Typeable
import GHC.Exts ( IsString(..) )
import GHC.IO.Encoding ( getFileSystemEncoding )
import GHC.IO.Exception (IOErrorType(InvalidArgument) )
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift(..), lift)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import System.IO.Error (catchIOError)

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
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
toAbstractFilePath = AbstractFilePath . WFP . encodeUtf16LE
#else
toAbstractFilePath = AbstractFilePath . PFP . encodeUtf8
#endif


-- | Like 'toAbstractFilePath', except on unix this uses the current
-- locale for encoding instead of always UTF8.
--
-- Looking up the locale requires IO. If you're not worried about calls
-- to 'setFileSystemEncoding', then 'unsafePerformIO' may be feasible.
toAbstractFilePath' :: String -> IO AbstractFilePath
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
toAbstractFilePath' = AbstractFilePath . WFP . encodeUtf16LE
#else
toAbstractFilePath' str = do
  enc <- getFileSystemEncoding
  cstr <- GHC.newCString enc str
  AbstractFilePath . PFP <$> BS.packCString cstr
#endif


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
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
fromAbstractFilePath (AbstractFilePath (WFP ba)) = either throwM pure $ decodeUtf16LE' ba
#else
fromAbstractFilePath (AbstractFilePath (PFP ba)) = either throwM pure $ decodeUtf8' ba
#endif


-- | Like 'fromAbstractFilePath', except on unix this uses the current
-- locale for decoding instead of always UTF8.
--
-- Looking up the locale requires IO. If you're not worried about calls
-- to 'setFileSystemEncoding', then 'unsafePerformIO' may be feasible.
--
-- Throws 'UnicodeException' if decoding fails.
fromAbstractFilePath' :: AbstractFilePath -> IO String
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
fromAbstractFilePath' (AbstractFilePath (WFP ba)) = either throwIO pure $ decodeUtf16LE' ba
#else
fromAbstractFilePath' (AbstractFilePath (PFP ba)) = flip catchIOError (\_ -> throwIO (DecodeError "fromAbstractFilePath' failed" Nothing))
  $ BS.useAsCString ba $ \fp -> getFileSystemEncoding >>= \enc -> GHC.peekCString enc fp
#endif


-- | Constructs an @AbstractFilePath@ from a ByteString.
--
-- On windows, this ensures valid UTF16, on unix it is passed unchanged/unchecked.
--
-- Throws 'UnicodeException' on invalid UTF16 on windows.
fromByteString :: MonadThrow m
               => ByteString
               -> m AbstractFilePath
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
fromByteString bs =
  either throwM (const . pure . AbstractFilePath . WFP . BS.toShort $ bs) $ decodeUtf16LE'' bs
#else
fromByteString = pure . AbstractFilePath . PFP . BS.toShort
#endif


qq :: (ByteString -> Q Exp) -> QuasiQuoter
qq quoteExp' =
  QuasiQuoter
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
  { quoteExp  = (\s -> quoteExp' . E.encodeUtf16LE . T.pack $ s)
#else
  { quoteExp  = (\s -> quoteExp' . E.encodeUtf8 . T.pack $ s)
#endif
  , quotePat  = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a pattern)"
  , quoteType = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a type)"
  , quoteDec  = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a declaration)"
  }

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
absFP :: QuasiQuoter
absFP = qq mkAbstractFilePath

