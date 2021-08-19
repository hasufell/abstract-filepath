{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- This template expects CPP definitions for:
--     MODULE_NAME = Posix | Windows
--     IS_WINDOWS  = False | True

module AFP.AbstractFilePath.Internal.MODULE_NAME where

-- doctest
import AFP.AbstractFilePath.Internal.Types
    ()

#ifdef WINDOWS
import qualified AFP.Data.ByteString.Short.Word16 as BS
import AFP.Data.Word16
    ( isLetter
    , isSpace
    , toLower
    , toUpper
    , _C
    , _N
    , _U
    , _asterisk
    , _backslash
    , _bar
    , _colon
    , _greater
    , _less
    , _nul
    , _period
    , _question
    , _quotedbl
    , _semicolon
    , _slash
    , _underscore
    )
import GHC.Word
    ( Word16 )
#else
import qualified AFP.Data.ByteString.Short as BS
import Data.Word8
    ( Word8
    , isLetter
    , isSpace
    , toLower
    , toUpper
    , _C
    , _N
    , _U
    , _asterisk
    , _backslash
    , _bar
    , _colon
    , _greater
    , _less
    , _nul
    , _period
    , _question
    , _quotedbl
    , _semicolon
    , _slash
    , _underscore
    )
#endif

import Control.Arrow
    ( second )
import Data.ByteString
    ( ByteString )
import Data.ByteString.Short
    ( ShortByteString )
import Data.List
    ( mapAccumL )
import Data.Maybe
    ( isJust )
import Prelude hiding
    ( Word )

import Data.String

#ifdef WINDOWS
type Word = Word16
#else
type Word = Word8
#endif


#ifdef WINDOWS
-- $setup
-- >>> :set -XFlexibleInstances
-- >>> import Prelude hiding (Word)
-- >>> import Data.ByteString.Short (ShortByteString)
-- >>> import Data.Char
-- >>> import Data.Maybe
-- >>> import Data.Word8
-- >>> import Test.QuickCheck
-- >>> import Control.Applicative
-- >>> import AFP.AbstractFilePath.Internal.Types
-- >>> import AFP.OsString.Internal.Types
-- >>> import qualified AFP.Data.ByteString.Short.Word16 as BS
-- >>> instance Arbitrary ShortByteString where arbitrary = BS.pack <$> arbitrary
-- >>> instance CoArbitrary ShortByteString where coarbitrary = coarbitrary . BS.unpack
-- >>> instance Arbitrary PosixFilePath where arbitrary = PS <$> arbitrary
-- >>> instance CoArbitrary PosixFilePath where coarbitrary = coarbitrary . (\(PS fp) -> fp)
-- >>> import AFP.OsString.Internal.Types (WindowsString (..))
-- >>> instance Arbitrary ShortByteString where arbitrary = sized $ \n -> choose (0,n) >>= \k -> fmap BS.pack $ vectorOf (if even k then k else k + 1) arbitrary
-- >>> instance Arbitrary WindowsString where arbitrary = WS <$> arbitrary
--
-- >>> let _chr :: Word -> Char; _chr = chr . fromIntegral
#else
-- $setup
-- >>> :set -XFlexibleInstances
-- >>> import Prelude hiding (Word)
-- >>> import AFP.AbstractFilePath.Internal.Types
-- >>> import Data.ByteString.Short (ShortByteString)
-- >>> import Data.Char
-- >>> import Data.Maybe
-- >>> import Data.Word8
-- >>> import Test.QuickCheck
-- >>> import Control.Applicative
-- >>> import AFP.OsString.Internal.Types
-- >>> import qualified Data.ByteString.Short as BS
-- >>> instance Arbitrary ShortByteString where arbitrary = BS.pack <$> arbitrary
-- >>> instance CoArbitrary ShortByteString where coarbitrary = coarbitrary . BS.unpack
-- >>> instance Arbitrary PosixFilePath where arbitrary = PS <$> arbitrary
-- >>> instance CoArbitrary PosixFilePath where coarbitrary = coarbitrary . (\(PS fp) -> fp)
-- >>> import AFP.OsString.Internal.Types (WindowsString (..))
-- >>> instance Arbitrary ShortByteString where arbitrary = sized $ \n -> choose (0,n) >>= \k -> fmap BS.pack $ vectorOf (if even k then k else k + 1) arbitrary
-- >>> instance Arbitrary WindowsString where arbitrary = WS <$> arbitrary
--
-- >>> let _chr :: Word -> Char; _chr = chr . fromIntegral
#endif

---------------------------------------------------------------------
-- Platform Abstraction Methods (private)

-- | Is the operating system Unix or Linux like
isPosix :: Bool
isPosix = not isWindows

-- | Is the operating system Windows like
isWindows :: Bool
isWindows = IS_WINDOWS


------------------------
-- Separator predicates


-- | Ideal path separator character
pathSeparator :: Word
pathSeparator = if isWindows
  then _backslash
  else _slash

-- | All path separator characters
pathSeparators :: [Word]
pathSeparators = if isWindows
  then [_backslash, _slash]
  else [_slash]

-- | Check if a character is the path separator
--
-- prop> \n ->  (_chr n == '/') == isPathSeparator n
isPathSeparator :: Word -> Bool
isPathSeparator w = w `elem` pathSeparators

-- | Like 'isPathSeparator'', except for 'ShortByteString'.
--
-- prop> \n ->  (_chr n == '/') == isPathSeparator' (BS.singleton n)
isPathSeparator' :: ShortByteString -> Bool
isPathSeparator' fp =
  BS.length fp == 1 && isPathSeparator (BS.head fp)


-- | Search path separator
searchPathSeparator :: Word
searchPathSeparator = if isWindows
  then _semicolon
  else _colon


#ifdef WINDOWS
-- | Check if a character is the search path separator
--
-- prop> \n -> (_chr n == ';') == isSearchPathSeparator n
#else
-- prop> \n -> (_chr n == ':') == isSearchPathSeparator n
#endif
isSearchPathSeparator :: Word -> Bool
isSearchPathSeparator = (== searchPathSeparator)


-- | File extension separator
extSeparator :: Word
extSeparator = _period


-- | Check if a character is the file extension separator
--
-- prop> \n -> (_chr n == '.') == isExtSeparator n
isExtSeparator :: Word -> Bool
isExtSeparator = (== extSeparator)



------------------------
-- $PATH methods


splitSearchPath :: ShortByteString -> [ShortByteString]
splitSearchPath = f
  where
    f bs = let (pre, post) = BS.break isSearchPathSeparator bs
           in if BS.null post
                 then g pre
                 else g pre ++ f (BS.tail post)
    g x
      | BS.null x = [BS.singleton _period | isPosix]
      | BS.length x >=2
      && BS.head x == _quotedbl
      && isWindows
      && BS.last x == _quotedbl = [BS.tail $ BS.init x]
      | otherwise = [x]



------------------------
-- Extension functions

splitExtension :: ShortByteString -> (ShortByteString, ShortByteString)
splitExtension x = if BS.null basename || (BS.singleton extSeparator == basename)
    then (x, BS.empty)
    else (BS.append path (BS.init basename), BS.cons extSeparator fileExt)
  where
    (path, file) = splitFileNameRaw x
    (basename, fileExt) = BS.breakEnd isExtSeparator file


takeExtension :: ShortByteString -> ShortByteString
takeExtension = snd . splitExtension


replaceExtension :: ShortByteString -> ShortByteString -> ShortByteString
replaceExtension path ext = dropExtension path <.> ext


dropExtension :: ShortByteString -> ShortByteString
dropExtension = fst . splitExtension


addExtension :: ShortByteString -> ShortByteString -> ShortByteString
addExtension file ext
    | BS.null ext = file
    | isExtSeparator (BS.head ext) = joinDrive a (BS.append b ext)
    | otherwise = joinDrive a (BS.intercalate (BS.singleton extSeparator) [b, ext])
    where
      (a, b) = splitDrive file


hasExtension :: ShortByteString -> Bool
hasExtension = isJust . BS.elemIndex extSeparator . takeFileName


(<.>) :: ShortByteString -> ShortByteString -> ShortByteString
(<.>) = addExtension


splitExtensions :: ShortByteString -> (ShortByteString, ShortByteString)
splitExtensions x = if BS.null basename
    then (path, fileExt)
    else (BS.append path basename, fileExt)
  where
    (path, file) = splitFileNameRaw x
    (basename, fileExt) = BS.break isExtSeparator file


dropExtensions :: ShortByteString -> ShortByteString
dropExtensions = fst . splitExtensions


takeExtensions :: ShortByteString -> ShortByteString
takeExtensions = snd . splitExtensions


stripExtension :: ShortByteString -> ShortByteString -> Maybe ShortByteString
stripExtension bs path
  | BS.null bs = Just path
  | otherwise  = BS.stripSuffix dotExt path
  where
    dotExt = if isExtSeparator $ BS.head bs
                then bs
                else extSeparator `BS.cons` bs


------------------------
-- Filename/directory functions


splitFileName :: ShortByteString -> (ShortByteString, ShortByteString)
splitFileName x = if BS.null path
    then (dotSlash, file)
    else (path, file)
  where
    (path, file) = splitFileNameRaw x
    dotSlash = _period `BS.cons` (BS.singleton pathSeparator)


takeFileName :: ShortByteString -> ShortByteString
takeFileName = snd . splitFileName -- TODO: null filename not allowed


replaceFileName :: ShortByteString -> ShortByteString -> ShortByteString
replaceFileName x y = fst (splitFileNameRaw x) </> y


dropFileName :: ShortByteString -> ShortByteString
dropFileName = fst . splitFileName


takeBaseName :: ShortByteString -> ShortByteString
takeBaseName = dropExtension . takeFileName


replaceBaseName :: ShortByteString -> ShortByteString -> ShortByteString
replaceBaseName path name = combineRaw dir (name <.> ext)
  where
    (dir,file) = splitFileNameRaw path
    ext = takeExtension file


takeDirectory :: ShortByteString -> ShortByteString
takeDirectory x = case () of
    () | isPathSeparator' x -> x
       | BS.null res && not (BS.null file) -> file
       | otherwise -> res
  where
    res = fst $ BS.spanEnd isPathSeparator file
    file = dropFileName x


replaceDirectory :: ShortByteString -> ShortByteString -> ShortByteString
replaceDirectory file dir = combineRaw dir (takeFileName file)


combine :: ShortByteString -> ShortByteString -> ShortByteString
combine a b | not (BS.null b) && isPathSeparator (BS.head b) || hasDrive b = b
            | otherwise = combineRaw a b


(</>) :: ShortByteString -> ShortByteString -> ShortByteString
(</>) = combine

splitPath :: ShortByteString -> [ShortByteString]
splitPath x = [drive | not (BS.null drive)] ++ f path
    where
        (drive, path) = splitDrive x

        f y
          | BS.null y = []
          | otherwise = (a `BS.append` c) : f d
            where
                (a,b) = BS.break isPathSeparator y
                (c,d) = BS.span  isPathSeparator b


joinPath :: [ShortByteString] -> ShortByteString
joinPath = foldr (</>) mempty


splitDirectories :: ShortByteString -> [ShortByteString]
splitDirectories x
    | BS.null x = []
    | isPathSeparator (BS.head x) = let (root,rest) = BS.splitAt 1 x
                                    in root : splitter rest
    | otherwise = splitter x
  where
    splitter = filter (not . BS.null) . BS.splitWith isPathSeparator


takeAllParents :: ShortByteString -> [ShortByteString]
takeAllParents x =
  let s = splitDirectories x
  in filterEmptyHead
       . snd
       . mapAccumL (\a b -> (if | BS.null a          -> (                                   b, a                         )
                                | isPathSeparator' a -> (     BS.singleton pathSeparator <> b, BS.singleton pathSeparator)
                                | otherwise          -> (a <> BS.singleton pathSeparator <> b, a                         )
                            )
                   ) mempty
       $ s
 where
  filterEmptyHead :: [ShortByteString] -> [ShortByteString]
  filterEmptyHead [] = []
  filterEmptyHead (a:as)
    | BS.null a = as
    | otherwise = (a:as)


------------------------
-- Trailing slash functions

hasTrailingPathSeparator :: ShortByteString -> Bool
hasTrailingPathSeparator x
  | BS.null x = False
  | otherwise = isPathSeparator $ BS.last x


addTrailingPathSeparator :: ShortByteString -> ShortByteString
addTrailingPathSeparator bs = if hasTrailingPathSeparator bs
    then bs
    else BS.snoc bs pathSeparator 


dropTrailingPathSeparator :: ShortByteString -> ShortByteString
dropTrailingPathSeparator x =
    if hasTrailingPathSeparator x && if isPosix then True else not (isDrive x)
    then let x' = BS.dropWhileEnd isPathSeparator x
         in if BS.null x' then BS.singleton (BS.last x) else x'
    else x



------------------------
-- File name manipulations


normalise :: ShortByteString -> ShortByteString
normalise filepath =
  result `BS.append`
  (if addPathSeparator
       then BS.singleton pathSeparator
       else BS.empty)
  where
    (drv,pth) = splitDrive filepath

    result = joinDrive' (normaliseDrive drv) (f pth)

    joinDrive' d p
      = if BS.null d && BS.null p
           then BS.singleton _period
           else joinDrive d p

    addPathSeparator = isDirPath filepath
      && not (hasTrailingPathSeparator result)
      && not (isRelativeDrive drv)

    isDirPath xs = hasTrailingPathSeparator xs
        || not (BS.null xs) && BS.last xs == _period
           && hasTrailingPathSeparator (BS.init xs)

    f = joinPath . dropDots . propSep . splitDirectories

    propSep :: [ShortByteString] -> [ShortByteString]
    propSep (x:xs)
      | BS.all isPathSeparator x = BS.singleton pathSeparator : xs
      | otherwise                   = x : xs
    propSep [] = []

    dropDots :: [ShortByteString] -> [ShortByteString]
    dropDots = filter (BS.singleton _period /=)


normaliseDrive :: ShortByteString -> ShortByteString
normaliseDrive bs
  | BS.null bs = mempty
  | isPosix = BS.pack [pathSeparator]
  | otherwise = if isJust $ readDriveLetter x2
         then BS.map toUpper x2
         else x2
    where
        x2 = BS.map repSlash bs
        repSlash x = if isPathSeparator x then pathSeparator else x


makeRelative :: ShortByteString -> ShortByteString -> ShortByteString
makeRelative root path
  | equalFilePath root path = BS.singleton _period
  | takeAbs root /= takeAbs path = path
  | otherwise = f (dropAbs root) (dropAbs path)
  where
    f x y
      | BS.null x = BS.dropWhile isPathSeparator y
      | otherwise = let (x1,x2) = g x
                        (y1,y2) = g y
                    in if equalFilePath x1 y1 then f x2 y2 else path
    g x = (BS.dropWhile isPathSeparator a, BS.dropWhile isPathSeparator b)
      where (a, b) = BS.break isPathSeparator $ BS.dropWhile isPathSeparator x

    -- on windows, need to drop '/' which is kind of absolute, but not a drive
    dropAbs x | BS.length x >= 1 && isPathSeparator (BS.head x) && not (hasDrive x) = BS.tail x
    dropAbs x = dropDrive x

    takeAbs x | BS.length x >= 1 && isPathSeparator (BS.head x) && not (hasDrive x) = BS.singleton pathSeparator
    takeAbs x = BS.map (\y -> if isPathSeparator y then pathSeparator else toLower y) $ takeDrive x


equalFilePath :: ShortByteString -> ShortByteString -> Bool
equalFilePath p1 p2 = f p1 == f p2
  where
    f x | isWindows = dropTrailingPathSeparator $ BS.map toLower $ normalise x
        | otherwise = dropTrailingPathSeparator $ normalise x


isRelative :: ShortByteString -> Bool
isRelative x = BS.null drive || isRelativeDrive drive
    where drive = takeDrive x

{- c:foo -}
-- From [1]: "If a file name begins with only a disk designator but not the
-- backslash after the colon, it is interpreted as a relative path to the
-- current directory on the drive with the specified letter."
isRelativeDrive :: ShortByteString -> Bool
isRelativeDrive x =
    maybe False (not . hasTrailingPathSeparator . fst) (readDriveLetter x)


isAbsolute :: ShortByteString -> Bool
isAbsolute = not .isRelative


-- Information for validity functions on Windows. See [1].
isBadCharacter :: Word -> Bool
isBadCharacter x = x >= _nul && x <= 31
  || x `elem`
      [ _less
      , _greater
      , _colon
      , _quotedbl
      , _bar
      , _question
      , _asterisk
      ]

badElements :: [ShortByteString]
badElements = fmap fromString
    ["CON","PRN","AUX","NUL","CLOCK$"
    ,"COM1","COM2","COM3","COM4","COM5","COM6","COM7","COM8","COM9"
    ,"LPT1","LPT2","LPT3","LPT4","LPT5","LPT6","LPT7","LPT8","LPT9"]

isValid :: ShortByteString -> Bool
isValid path
  | BS.null path = False
  | _nul `BS.elem` path = False
  | isPosix = True
  | otherwise =
      not (BS.any isBadCharacter x2) &&
      not (any f $ splitDirectories x2) &&
      not (isJust (readDriveShare x1) && BS.all isPathSeparator x1) &&
      not (isJust (readDriveUNC x1) && not (hasTrailingPathSeparator x1))
    where
      (x1,x2) = splitDrive path
      f x = BS.map toUpper (BS.dropWhileEnd isSpace $ dropExtensions x) `elem` badElements


makeValid :: ShortByteString -> ShortByteString
makeValid path
  | BS.null path = BS.singleton _underscore
  | isPosix = BS.map (\x -> if x == _nul then _underscore else x) path
  | isJust (readDriveShare drv) && BS.all isPathSeparator drv = BS.take 2 drv `BS.append` (fromString "drive")
  | isJust (readDriveUNC drv) && not (hasTrailingPathSeparator drv) =
      makeValid (drv `BS.append` (BS.singleton pathSeparator) `BS.append` pth)
  | otherwise = joinDrive drv $ validElements $ validChars pth

  where
    (drv,pth) = splitDrive path

    validChars = BS.map f
    f x = if isBadCharacter x then _underscore else x

    validElements = joinPath . map g . splitPath
    g x = h a `BS.append` b
        where (a,b) = BS.break isPathSeparator x
    h x = if BS.map toUpper (BS.dropWhileEnd isSpace a) `elem` badElements then (BS.snoc a _underscore ) <.> b else x
        where (a,b) = splitExtensions x


isFileName :: ShortByteString -> Bool
isFileName filepath =
  not (foldr (\a b -> BS.singleton a `BS.isInfixOf` filepath || b) False pathSeparators) &&
  not (BS.null filepath) &&
  not (_nul `BS.elem` filepath)

#ifndef WINDOWS
hiddenFile :: ShortByteString -> Bool
hiddenFile fp
  | fn == BS.pack [_period, _period] = False
  | fn == BS.pack [_period]          = False
  | otherwise                        = BS.pack [extSeparator]
                                         `BS.isPrefixOf` fn
  where
    fn = takeFileName fp
#endif


hasParentDir :: ShortByteString -> Bool
hasParentDir filepath =
    predicate (`BS.cons` pathDoubleDot)
     BS.isSuffixOf
   ||
    predicate (\sep -> BS.singleton sep
        `BS.append` pathDoubleDot
        `BS.append` BS.singleton sep)
     BS.isInfixOf
   ||
    predicate (\sep -> BS.snoc pathDoubleDot sep)
      BS.isPrefixOf
  where
    pathDoubleDot = BS.pack [_period, _period]
    predicate f p =
      foldr (\a b -> f a
              `p` filepath || b)
            False
            pathSeparators


------------------------
-- internal stuff

splitFileNameRaw :: ShortByteString -> (ShortByteString, ShortByteString)
splitFileNameRaw fp = (BS.append drv dir, file)
  where
    (drv, pth) = splitDrive fp
    (dir, file) = BS.breakEnd isPathSeparator pth


combineRaw :: ShortByteString -> ShortByteString -> ShortByteString
combineRaw a b | BS.null a = b
               | BS.null b = a
               | isPathSeparator (BS.last a) = BS.append a b
               | BS.length a == 2
               && BS.last a == _colon
               && isWindows
               && isLetter (BS.head a)
               = BS.append a b
               | otherwise = BS.intercalate (BS.singleton pathSeparator) [a, b]


---------------------------------------------------------------------
-- Drive methods

splitDrive :: ShortByteString -> (ShortByteString, ShortByteString)
splitDrive x | isPosix = BS.span (== _slash) x
splitDrive x | Just y <- readDriveLetter x = y
splitDrive x | Just y <- readDriveUNC x = y
splitDrive x | Just y <- readDriveShare x = y
splitDrive x = (mempty, x)

addSlash :: ShortByteString -> ShortByteString -> (ShortByteString, ShortByteString)
addSlash a xs = (BS.append a c,d)
    where (c,d) = BS.span isPathSeparator xs

-- See [1].
-- "\\?\D:\<path>" or "\\?\UNC\<server>\<share>"
readDriveUNC :: ShortByteString -> Maybe (ShortByteString, ShortByteString)
readDriveUNC bs = case BS.unpack bs of
  (s1:s2:q:s3:xs)
    | q == _question && all isPathSeparator [s1,s2,s3] ->
      case map toUpper xs of
          (u:n:c:s4:_)
            | u == _U && n == _N && c == _C && isPathSeparator s4 ->
              let (a,b) = readDriveShareName (BS.pack (drop 4 xs))
              in Just (BS.pack (s1:s2:_question:s3:take 4 xs) `BS.append` a, b)
          _ -> case readDriveLetter (BS.pack xs) of
                   -- Extended-length path.
                   Just (a,b) -> Just (BS.pack (s1:s2:_question:s3:[]) `BS.append` a, b)
                   Nothing -> Nothing
  _ -> Nothing

{- c:\ -}
readDriveLetter :: ShortByteString -> Maybe (ShortByteString, ShortByteString)
readDriveLetter bs = case BS.unpack bs of
  (x:c:y:xs)
    | c == _colon && isLetter x && isPathSeparator y -> Just $ addSlash (BS.pack [x,_colon]) (BS.pack (y:xs))
  (x:c:xs)
    | c == _colon && isLetter x -> Just (BS.pack [x,_colon], BS.pack xs)
  _ -> Nothing

{- \\sharename\ -}
readDriveShare :: ShortByteString -> Maybe (ShortByteString, ShortByteString)
readDriveShare bs = case BS.unpack bs of
  (s1:s2:xs) | isPathSeparator s1 && isPathSeparator s2 -> 
    let (a, b) = readDriveShareName (BS.pack xs)
    in Just (s1 `BS.cons` s2 `BS.cons` a,b)
  _ -> Nothing

{- assume you have already seen \\ -}
{- share\bob -> "share\", "bob" -}
readDriveShareName :: ShortByteString -> (ShortByteString, ShortByteString)
readDriveShareName name = addSlash a b
    where (a,b) = BS.break isPathSeparator name


joinDrive :: ShortByteString -> ShortByteString -> ShortByteString
joinDrive = combineRaw


takeDrive :: ShortByteString -> ShortByteString
takeDrive = fst . splitDrive

dropDrive :: ShortByteString -> ShortByteString
dropDrive = snd . splitDrive

hasDrive :: ShortByteString -> Bool
hasDrive = not . BS.null . takeDrive

isDrive :: ShortByteString -> Bool
isDrive x = not (BS.null x) && BS.null (dropDrive x)
