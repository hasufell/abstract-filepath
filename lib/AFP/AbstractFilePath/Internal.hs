{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnliftedFFITypes #-}

module AFP.AbstractFilePath.Internal where

import AFP.AbstractFilePath.Internal.Types
import AFP.OsString.Internal.Types
import AFP.OsString.Internal hiding
    ( fromByteString )
import qualified AFP.OsString.Internal as OS

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
toAbstractFilePathIO :: String -> IO AbstractFilePath
toAbstractFilePathIO = toOsStringIO


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
fromAbstractFilePathIO :: AbstractFilePath -> IO String
fromAbstractFilePathIO = fromOsStringIO


-- | Constructs an @AbstractFilePath@ from a ByteString.
--
-- On windows, this ensures valid UTF16, on unix it is passed unchanged/unchecked.
--
-- Throws 'UnicodeException' on invalid UTF16 on windows.
bsToAFP :: MonadThrow m
        => ByteString
        -> m AbstractFilePath
bsToAFP = OS.bsToOsString


mkAbstractFilePath :: ByteString -> Q Exp
mkAbstractFilePath bs = 
  case bsToAFP bs of
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


unpackAFP :: AbstractFilePath -> [OsWord]
unpackAFP = unpackOsString


packAFP :: [OsWord] -> AbstractFilePath
packAFP = packOsString

