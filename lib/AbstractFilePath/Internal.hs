{-# LANGUAGE CPP, RankNTypes, UnliftedFFITypes, TemplateHaskell #-}

module AbstractFilePath.Internal where

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


-- Using unpinned bytearrays to avoid Heap fragmentation and
-- which are reasonably cheap to pass to FFI calls
-- wrapped with typeclass-friendly types allowing to avoid CPP
-- 
-- Note that, while unpinned bytearrays incur a memcpy on each
-- FFI call, this overhead is generally much preferable to
-- the memory fragmentation of pinned bytearrays

-- | Filepaths are UTF16 data on windows as passed to syscalls.
data WindowsFilePath = WFP BS.ShortByteString 
  deriving (Eq, Ord, Show)
-- | Filepaths are @char[]@ data on unix as passed to syscalls.
data PosixFilePath   = PFP BS.ShortByteString
  deriving (Eq, Ord, Show)

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
type PlatformFilePath = WindowsFilePath
#else
type PlatformFilePath = PosixFilePath
#endif

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


-- | This is a fuzzy check whether a filepath is valid.
--
-- On /Unix/, this
-- checks only for the absence of NUL bytes according to the <https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap03.html#tag_03_170 POSIX specification>.
--
-- On /Windows/, this does a best effort following the <https://docs.microsoft.com/en-us/windows/win32/fileio/naming-a-file#naming-conventions naming conventions>, which
-- is not exhaustive.
--
-- Further filestysem restrictions may apply. Use this function with caution,
-- preferably with user input. Don't run this on filepaths *returned* by a syscall.
filepathIsValid :: AbstractFilePath
                -> Bool
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
filepathIsValid (AbstractFilePath (WFP ba)) =
  (and . fmap (not . flip elem notPermittedChars) . BS.unpack $ ba)
  &&
  (not $ elem ba notPermittedNames)
  &&
  (not $ BS.null ba)
  where
    notPermittedChars =
      [ _nul
      , _less
      , _greater
      , _colon
      , _quotedbl
      , _slash
      , _bar
      , _question
      , _asterisk
      ]
    notPermittedNames = fromString <$>
      [ "CON"
      , "PRN"
      , "AUX"
      , "NUL"
      , "COM1"
      , "COM2"
      , "COM3"
      , "COM4"
      , "COM5"
      , "COM6"
      , "COM7"
      , "COM8"
      , "COM9"
      , "LPT1"
      , "LPT2"
      , "LPT3"
      , "LPT4"
      , "LPT5"
      , "LPT6"
      , "LPT7"
      , "LPT8"
      , "LPT9"
      ]
#else
filepathIsValid (AbstractFilePath (PFP ba))
  = (not $ elem _nul $ BS.unpack ba)
  && (not . BS.null $ ba)
#endif


-- | Type representing filenames\/pathnames.
--
-- Internally this is either 'WindowsFilePath' or 'PosixFilePath',
-- depending on the platform. Both use unpinned
-- 'ShortByteString' for efficiency and correctness.
--
-- The constructor is only exported via "AbstractFilePath.Internal", since
-- dealing with the internals isn't generally recommended, but supported
-- in case you need to write platform specific code, such as the implementation
-- of 'fromAbstractFilePath'.
newtype AbstractFilePath = AbstractFilePath PlatformFilePath
  deriving Show

-- | Byte equality of the internal representation.
instance Eq AbstractFilePath where
  (AbstractFilePath a) == (AbstractFilePath b) = a == b

-- | Byte ordering of the internal representation.
instance Ord AbstractFilePath where
  compare (AbstractFilePath a) (AbstractFilePath b) = compare a b

-- | Encodes as UTF16 on windows and UTF8 on unix.
instance IsString AbstractFilePath where 
    fromString = toAbstractFilePath

-- | \"String-Concatenation\" for 'AbstractFilePath'. This is __not__ the same
-- as '(</>)'.
instance Monoid AbstractFilePath where 
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
    mempty      = AbstractFilePath (WFP BS.empty)
    mappend (AbstractFilePath (WFP a)) (AbstractFilePath (WFP b))
      = AbstractFilePath (WFP (mappend a b))
#else
    mempty      = AbstractFilePath (PFP BS.empty)
    mappend (AbstractFilePath (PFP a)) (AbstractFilePath (PFP b))
      = AbstractFilePath (PFP (mappend a b))
#endif
#if MIN_VERSION_base(4,11,0)
instance Semigroup AbstractFilePath where 
    (<>) = mappend
#endif


instance Lift AbstractFilePath where
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
  lift (AbstractFilePath (WFP bs))
    = [| AbstractFilePath (WFP (BS.pack $(lift $ BS.unpack bs))) :: AbstractFilePath |]
#else
  lift (AbstractFilePath (PFP bs))
    = [| AbstractFilePath (PFP (BS.pack $(lift $ BS.unpack bs))) :: AbstractFilePath |]
#endif
#if MIN_VERSION_template_haskell(2,17,0)
  liftTyped = TH.unsafeCodeCoerce . TH.lift
#elif MIN_VERSION_template_haskell(2,16,0)
  liftTyped = TH.unsafeTExpCoerce . TH.lift
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
      if filepathIsValid afp
      then lift afp
      else error "invalid filepath"
    Nothing -> error "invalid encoding"

-- | QuasiQuote an 'AbstractFilePath'. This accepts Unicode characters
-- and encodes as UTF-8 on unix and UTF-16 on windows. Runs 'filepathIsValid'
-- on the input.
absFP :: QuasiQuoter
absFP = qq mkAbstractFilePath

