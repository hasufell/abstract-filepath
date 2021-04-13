{-# LANGUAGE CPP #-}

module AbstractFilePath.Posix where


import AbstractFilePath.Internal.Types ( PosixFilePath(..) )

import Control.Arrow (second)
import Data.ByteString (ByteString)
import Data.Maybe (isJust)
import Data.Word8
    ( Word8, _colon, _nul, _period, _slash, _underscore )
import Data.ByteString.Short (ShortByteString)

import qualified Data.ByteString.Short as BS
import qualified AbstractFilePath.ShortByteString as BS


-- $setup
-- >>> import Data.Char
-- >>> import Data.Maybe
-- >>> import Data.Word8
-- >>> import Test.QuickCheck
-- >>> import Control.Applicative
-- >>> import AbstractFilePath.Internal.Types (PosixFilePath (..))
-- >>> import qualified Data.ByteString.Short as BS
-- >>> instance Arbitrary ShortByteString where arbitrary = BS.pack <$> arbitrary
-- >>> instance CoArbitrary ShortByteString where coarbitrary = coarbitrary . BS.unpack
-- >>> instance Arbitrary PosixFilePath where arbitrary = PFP <$> arbitrary
-- >>> instance CoArbitrary PosixFilePath where coarbitrary = coarbitrary . (\(PFP fp) -> fp)
--
-- >>> let _chr :: Word8 -> Char; _chr = chr . fromIntegral



------------------------
-- Separator predicates


-- | Ideal path separator character
pathSeparator :: Word8
pathSeparator = _slash

-- | Path separator character
pathSeparators :: [Word8]
pathSeparators = [_slash]

-- | Check if a character is the path separator
--
-- prop> \n ->  (_chr n == '/') == isPathSeparator n
isPathSeparator :: Word8 -> Bool
isPathSeparator = flip elem pathSeparators


isPathSeparator' :: PosixFilePath -> Bool
isPathSeparator' pfp@(PFP fp) =
  BS.length fp == 1 && isPathSeparator (BS.head fp)


-- | Search path separator
searchPathSeparator :: Word8
searchPathSeparator = _colon


-- | Check if a character is the search path separator
--
-- prop> \n -> (_chr n == ':') == isSearchPathSeparator n
isSearchPathSeparator :: Word8 -> Bool
isSearchPathSeparator = (== searchPathSeparator)


-- | File extension separator. This isn't defined by the POSIX standard
-- and may not work as expected on non-ASCII compatible encoding.
extSeparator :: Word8
extSeparator = _period


-- | Check if a character is the file extension separator
--
-- prop> \n -> (_chr n == '.') == isExtSeparator n
isExtSeparator :: Word8 -> Bool
isExtSeparator = (== extSeparator)



------------------------
-- $PATH methods


-- | Take a ShortByteString, split it on the 'searchPathSeparator'.
-- Blank items are converted to @.@.
--
-- Follows the recommendations in
-- <http://www.opengroup.org/onlinepubs/009695399/basedefs/xbd_chap08.html>
--
-- >>> splitSearchPath "File1:File2:File3"
-- [PFP "File1",PFP "File2",PFP "File3"]
-- >>> splitSearchPath "File1::File2:File3"
-- [PFP "File1",PFP ".",PFP "File2",PFP "File3"]
-- >>> splitSearchPath ""
-- [PFP "."]
splitSearchPath :: ShortByteString -> [PosixFilePath]
splitSearchPath = fmap PFP . f
  where
    f bs = let (pre, post) = BS.break isSearchPathSeparator bs
           in if BS.null post
                 then g pre
                 else g pre ++ f (BS.tail post)
    g x
      | BS.null x = [BS.singleton _period]
      | otherwise = [x]



------------------------
-- Extension functions

-- | Split a 'PosixFilePath' into a path+filename and extension
--
-- >>> splitExtension "file.exe"
-- (PFP "file",".exe")
-- >>> splitExtension "file"
-- (PFP "file","")
-- >>> splitExtension "/path/file.tar.gz"
-- (PFP "/path/file.tar",".gz")
-- >>> splitExtension ".exe"
-- (PFP ".exe","")
--
-- prop> \path -> PFP (uncurry (\(PFP a) b -> BS.append a b) (splitExtension path)) == path
splitExtension :: PosixFilePath -> (PosixFilePath, ShortByteString)
splitExtension x = if BS.null basename || (BS.singleton extSeparator == basename)
    then (x, BS.empty)
    else (PFP $ BS.append path (BS.init basename), BS.cons extSeparator fileExt)
  where
    (PFP path, PFP file) = splitFileNameRaw x
    (basename, fileExt) = BS.breakEnd isExtSeparator file


-- | Get the final extension from a 'PosixFilePath'
--
-- >>> takeExtension "file.exe"
-- ".exe"
-- >>> takeExtension "file"
-- ""
-- >>> takeExtension "/path/file.tar.gz"
-- ".gz"
takeExtension :: PosixFilePath -> ShortByteString
takeExtension = snd . splitExtension


-- | Change a file's extension
--
-- prop> \path -> let ext = takeExtension path in replaceExtension path ext == path
replaceExtension :: PosixFilePath -> ShortByteString -> PosixFilePath
replaceExtension path ext = dropExtension path <.> ext


-- | Drop the final extension from a 'PosixFilePath'
--
-- >>> dropExtension ".exe"
-- PFP ".exe"
-- >>> dropExtension "file.exe"
-- PFP "file"
-- >>> dropExtension "file"
-- PFP "file"
-- >>> dropExtension "/path/file.tar.gz"
-- PFP "/path/file.tar"
dropExtension :: PosixFilePath -> PosixFilePath
dropExtension = fst . splitExtension


-- | Add an extension to a 'PosixFilePath'
--
-- >>> addExtension "file" ".exe"
-- PFP "file.exe"
-- >>> addExtension "file.tar" ".gz"
-- PFP "file.tar.gz"
-- >>> addExtension "/path/" ".ext"
-- PFP "/path/.ext"
addExtension :: PosixFilePath -> ShortByteString -> PosixFilePath
addExtension file@(PFP bs) ext
    | BS.null ext = file
    | isExtSeparator (BS.head ext) = PFP $ BS.append bs ext
    | otherwise = PFP $ BS.intercalate (BS.singleton extSeparator) [bs, ext]


-- | Check if a 'PosixFilePath' has an extension
--
-- >>> hasExtension "file"
-- False
-- >>> hasExtension "file.tar"
-- True
-- >>> hasExtension "/path.part1/"
-- False
hasExtension :: PosixFilePath -> Bool
hasExtension = isJust . BS.elemIndex extSeparator . (\(PFP fn) -> fn) . takeFileName


-- | Operator version of 'addExtension'
(<.>) :: PosixFilePath -> ShortByteString -> PosixFilePath
(<.>) = addExtension


-- | Split a 'PosixFilePath' on the first extension.
--
-- >>> splitExtensions "/path/file.tar.gz"
-- (PFP "/path/file",".tar.gz")
--
-- prop> \path -> uncurry addExtension (splitExtensions path) == path
splitExtensions :: PosixFilePath -> (PosixFilePath, ShortByteString)
splitExtensions x = if BS.null basename
    then (PFP path, fileExt)
    else (PFP $ BS.append path basename, fileExt)
  where
    (PFP path, PFP file) = splitFileNameRaw x
    (basename, fileExt) = BS.break isExtSeparator file


-- | Remove all extensions from a 'PosixFilePath'
--
-- >>> dropExtensions "/path/file.tar.gz"
-- PFP "/path/file"
dropExtensions :: PosixFilePath -> PosixFilePath
dropExtensions = fst . splitExtensions


-- | Take all extensions from a 'PosixFilePath'
--
-- >>> takeExtensions "/path/file.tar.gz"
-- ".tar.gz"
takeExtensions :: PosixFilePath -> ShortByteString
takeExtensions = snd . splitExtensions


-- | Drop the given extension from a FilePath, and the @\".\"@ preceding it.
-- Returns 'Nothing' if the FilePath does not have the given extension, or
-- 'Just' and the part before the extension if it does.
--
-- This function can be more predictable than 'dropExtensions',
-- especially if the filename might itself contain @.@ characters.
--
-- >>> stripExtension "hs.o" "foo.x.hs.o"
-- Just (PFP "foo.x")
-- >>> stripExtension "hi.o" "foo.x.hs.o"
-- Nothing
-- >>> stripExtension ".c.d" "a.b.c.d"
-- Just (PFP "a.b")
-- >>> stripExtension ".c.d" "a.b..c.d"
-- Just (PFP "a.b.")
-- >>> stripExtension "baz"  "foo.bar"
-- Nothing
-- >>> stripExtension "bar"  "foobar"
-- Nothing
--
-- prop> \path -> stripExtension "" path == Just path
-- prop> \path -> dropExtension path  == fromJust (stripExtension (takeExtension path) path)
-- prop> \path -> dropExtensions path == fromJust (stripExtension (takeExtensions path) path)
stripExtension :: ShortByteString -> PosixFilePath -> Maybe PosixFilePath
stripExtension bs (PFP path)
  | BS.null bs = Just (PFP path)
  | otherwise  = fmap PFP $ BS.stripSuffix dotExt path
  where
    dotExt = if isExtSeparator $ BS.head bs
                then bs
                else extSeparator `BS.cons` bs


------------------------
-- Filename/directory functions


-- | Split a 'PosixFilePath' into (path,file).  'combine' is the inverse
--
-- >>> splitFileName "path/file.txt"
-- (PFP "path/",PFP "file.txt")
-- >>> splitFileName "path/"
-- (PFP "path/",PFP "")
-- >>> splitFileName "file.txt"
-- (PFP "./",PFP "file.txt")
--
-- prop> \path -> uncurry combine (splitFileName path) == path || fst (splitFileName path) == "./"
splitFileName :: PosixFilePath -> (PosixFilePath, PosixFilePath)
splitFileName x = if BS.null path
    then (PFP dotSlash, PFP file)
    else (PFP path, PFP file)
  where
    (PFP path, PFP file) = splitFileNameRaw x
    dotSlash = _period `BS.cons` (BS.singleton pathSeparator)


-- | Get the file name
--
-- >>> takeFileName "path/file.txt"
-- PFP "file.txt"
-- >>> takeFileName "path/"
-- PFP ""
takeFileName :: PosixFilePath -> PosixFilePath
takeFileName = snd . splitFileName -- TODO: null filename not allowed


-- | Change the file name
--
-- prop> \path -> let (PFP fn) = takeFileName path in replaceFileName path fn == path
replaceFileName :: PosixFilePath -> ShortByteString -> PosixFilePath
replaceFileName x y = fst (splitFileNameRaw x) </> PFP y


-- | Drop the file name
--
-- >>> dropFileName "path/file.txt"
-- PFP "path/"
-- >>> dropFileName "file.txt"
-- PFP "./"
dropFileName :: PosixFilePath -> PosixFilePath
dropFileName = fst . splitFileName


-- | Get the file name, without a trailing extension
--
-- >>> takeBaseName "path/file.tar.gz"
-- PFP "file.tar"
-- >>> takeBaseName ""
-- PFP ""
takeBaseName :: PosixFilePath -> PosixFilePath
takeBaseName = dropExtension . takeFileName


-- | Change the base name
--
-- >>> replaceBaseName "path/file.tar.gz" "bob"
-- PFP "path/bob.gz"
--
-- prop> \path -> let (PFP baseName) = takeBaseName path in replaceBaseName path baseName == path
replaceBaseName :: PosixFilePath -> ShortByteString -> PosixFilePath
replaceBaseName path name = combineRaw dir (PFP name <.> ext)
  where
    (dir,file) = splitFileNameRaw path
    ext = takeExtension file


-- | Get the directory, moving up one level if it's already a directory
--
-- >>> takeDirectory "path/file.txt"
-- PFP "path"
-- >>> takeDirectory "file"
-- PFP "."
-- >>> takeDirectory "/path/to/"
-- PFP "/path/to"
-- >>> takeDirectory "/path/to"
-- PFP "/path"
takeDirectory :: PosixFilePath -> PosixFilePath
takeDirectory pfp@(PFP x) = case () of
    () | isPathSeparator' pfp -> PFP x
       | BS.null res && not (BS.null file) -> PFP file
       | otherwise -> PFP res
  where
    res = fst $ BS.spanEnd isPathSeparator file
    (PFP file) = dropFileName (PFP x)


-- | Change the directory component of a 'PosixFilePath'
--
-- prop> \path -> replaceDirectory path (takeDirectory path) `equalFilePath` path || takeDirectory path == "."
replaceDirectory :: PosixFilePath -> PosixFilePath -> PosixFilePath
replaceDirectory file dir = combineRaw dir (takeFileName file)


-- | Join two paths together. If the second path is absolute, then returns it, ignoring
--  the first path.
--
-- >>> combine "/" "file"
-- PFP "/file"
-- >>> combine "/path/to" "file"
-- PFP "/path/to/file"
-- >>> combine "file" "/absolute/path"
-- PFP "/absolute/path"
combine :: PosixFilePath -> PosixFilePath -> PosixFilePath
combine (PFP a) (PFP b) | not (BS.null b) && isPathSeparator (BS.head b) = PFP b
            | otherwise = combineRaw (PFP a) (PFP b)


-- | Operator version of combine
(</>) :: PosixFilePath -> PosixFilePath -> PosixFilePath
(</>) = combine

-- | Split a path into a list of components:
--
-- >>> splitPath "/path/to/file.txt"
-- [PFP "/",PFP "path/",PFP "to/",PFP "file.txt"]
--
-- prop> \path -> PFP (BS.concat (fmap (\(PFP fp) -> fp) (splitPath path))) == path
splitPath :: PosixFilePath -> [PosixFilePath]
splitPath (PFP bs) = fmap PFP $ splitter bs
  where
    splitter x
      | BS.null x = []
      | otherwise = case BS.findIndex isPathSeparator x of
            Nothing -> [x]
            Just ix -> case BS.findIndex (not . isPathSeparator) $ BS.drop (ix+1) x of
                          Nothing -> [x]
                          Just runlen -> uncurry (:) . second splitter $ BS.splitAt (ix+1+runlen) x


-- | Join a split path back together
--
-- prop> \path -> joinPath (splitPath path) == path
--
-- >>> joinPath ["path","to","file.txt"]
-- PFP "path/to/file.txt"
joinPath :: [PosixFilePath] -> PosixFilePath
joinPath = foldr (</>) (PFP mempty)


-- | Like 'splitPath', but without trailing slashes
--
-- >>> splitDirectories "/path/to/file.txt"
-- [PFP "/",PFP "path",PFP "to",PFP "file.txt"]
-- >>> splitDirectories "path/to/file.txt"
-- [PFP "path",PFP "to",PFP "file.txt"]
-- >>> splitDirectories "/"
-- [PFP "/"]
-- >>> splitDirectories ""
-- []
splitDirectories :: PosixFilePath -> [PosixFilePath]
splitDirectories (PFP x)
    | BS.null x = []
    | isPathSeparator (BS.head x) = let (root,rest) = BS.splitAt 1 x
                                    in fmap PFP (root : splitter rest)
    | otherwise = fmap PFP $ splitter x
  where
    splitter = filter (not . BS.null) . BS.splitWith isPathSeparator


-- |Get all parents of a path.
--
-- >>> takeAllParents "/abs/def/dod"
-- [PFP "/abs/def",PFP "/abs",PFP "/"]
-- >>> takeAllParents "/foo"
-- [PFP "/"]
-- >>> takeAllParents "/"
-- []
takeAllParents :: PosixFilePath -> [PosixFilePath]
takeAllParents p
  | isPathSeparator' np = []
  | otherwise = takeDirectory np : takeAllParents (takeDirectory np)
  where
    np = normalise p


------------------------
-- Trailing slash functions

-- | Check if the last character of a 'PosixFilePath' is '/'.
--
-- >>> hasTrailingPathSeparator "/path/"
-- True
-- >>> hasTrailingPathSeparator "/"
-- True
-- >>> hasTrailingPathSeparator "/path"
-- False
hasTrailingPathSeparator :: PosixFilePath -> Bool
hasTrailingPathSeparator (PFP x)
  | BS.null x = False
  | otherwise = isPathSeparator $ BS.last x


-- | Add a trailing path separator.
--
-- >>> addTrailingPathSeparator "/path"
-- PFP "/path/"
-- >>> addTrailingPathSeparator "/path/"
-- PFP "/path/"
-- >>> addTrailingPathSeparator "/"
-- PFP "/"
addTrailingPathSeparator :: PosixFilePath -> PosixFilePath
addTrailingPathSeparator x@(PFP bs) = if hasTrailingPathSeparator x
    then x
    else PFP (BS.snoc pathSeparator bs)


-- | Remove a trailing path separator
--
-- >>> dropTrailingPathSeparator "/path/"
-- PFP "/path"
-- >>> dropTrailingPathSeparator "/path////"
-- PFP "/path"
-- >>> dropTrailingPathSeparator "/"
-- PFP "/"
-- >>> dropTrailingPathSeparator "//"
-- PFP "/"
dropTrailingPathSeparator :: PosixFilePath -> PosixFilePath
dropTrailingPathSeparator pfp@(PFP x)
  | isPathSeparator' pfp = PFP x
  | otherwise = if hasTrailingPathSeparator pfp
                  then dropTrailingPathSeparator $ PFP $ BS.init x
                  else pfp



------------------------
-- File name manipulations


-- |Normalise a file.
--
-- >>> normalise "/file/\\test////"
-- PFP "/file/\\test/"
-- >>> normalise "/file/./test"
-- PFP "/file/test"
-- >>> normalise "/test/file/../bob/fred/"
-- PFP "/test/file/../bob/fred/"
-- >>> normalise "../bob/fred/"
-- PFP "../bob/fred/"
-- >>> normalise "./bob/fred/"
-- PFP "bob/fred/"
-- >>> normalise "./bob////.fred/./...///./..///#."
-- PFP "bob/.fred/.../../#."
-- >>> normalise "."
-- PFP "."
-- >>> normalise "./"
-- PFP "./"
-- >>> normalise "./."
-- PFP "./"
-- >>> normalise "/./"
-- PFP "/"
-- >>> normalise "/"
-- PFP "/"
-- >>> normalise "bob/fred/."
-- PFP "bob/fred/"
-- >>> normalise "//home"
-- PFP "/home"
normalise :: PosixFilePath -> PosixFilePath
normalise pfp@(PFP filepath) = PFP $
  result `BS.append`
  (if addPathSeparator
       then BS.singleton pathSeparator
       else BS.empty)
  where
    result = let (PFP n) = f pfp
             in if BS.null n
                then BS.singleton _period
                else n
    addPathSeparator = isDirPath pfp &&
      not (hasTrailingPathSeparator $ PFP result)
    isDirPath (PFP xs) = hasTrailingPathSeparator (PFP xs)
        || not (BS.null xs) && BS.last xs == _period
           && hasTrailingPathSeparator (PFP (BS.init xs))
    f = joinPath . fmap PFP . dropDots . propSep . fmap (\(PFP fp) -> fp) . splitDirectories
    propSep :: [ShortByteString] -> [ShortByteString]
    propSep (x:xs)
      | BS.all isPathSeparator x = BS.singleton pathSeparator : xs
      | otherwise                   = x : xs
    propSep [] = []
    dropDots :: [ShortByteString] -> [ShortByteString]
    dropDots = filter (BS.singleton _period /=)



-- | Contract a filename, based on a relative path. Note that the resulting
-- path will never introduce @..@ paths, as the presence of symlinks
-- means @..\/b@ may not reach @a\/b@ if it starts from @a\/c@. For a
-- worked example see
-- <http://neilmitchell.blogspot.co.uk/2015/10/filepaths-are-subtle-symlinks-are-hard.html this blog post>.
--
-- >>> makeRelative "/directory" "/directory/file.ext"
-- PFP "file.ext"
-- >>> makeRelative "/Home" "/home/bob"
-- PFP "/home/bob"
-- >>> makeRelative "/home/" "/home/bob/foo/bar"
-- PFP "bob/foo/bar"
-- >>> makeRelative "/fred" "bob"
-- PFP "bob"
-- >>> makeRelative "/file/test" "/file/test/fred"
-- PFP "fred"
-- >>> makeRelative "/file/test" "/file/test/fred/"
-- PFP "fred/"
-- >>> makeRelative "some/path" "some/path/a/b/c"
-- PFP "a/b/c"
--
-- prop> \p -> makeRelative p p == "."
-- prop> \p -> makeRelative (takeDirectory p) p `equalFilePath` takeFileName p
-- prop \x y -> equalFilePath x y || (isRelative x && makeRelative y x == x) || equalFilePath (y </> makeRelative y x) x
makeRelative :: PosixFilePath -> PosixFilePath -> PosixFilePath
makeRelative root@(PFP root') path@(PFP path')
  | equalFilePath root path = PFP $ BS.singleton _period
  | takeAbs root' /= takeAbs path' = path
  | otherwise = PFP $ f (dropAbs root') (dropAbs path')
  where
    f x y
      | BS.null x = BS.dropWhile isPathSeparator y
      | otherwise = let (x1,x2) = g x
                        (y1,y2) = g y
                    in if equalFilePath (PFP x1) (PFP y1) then f x2 y2 else path'
    g x = (BS.dropWhile isPathSeparator a, BS.dropWhile isPathSeparator b)
      where (a, b) = BS.break isPathSeparator $ BS.dropWhile isPathSeparator x
    dropAbs x = snd $ BS.span isPathSeparator x
    takeAbs x = fst $ BS.span isPathSeparator x


-- |Equality of two filepaths. The filepaths are normalised
-- and trailing path separators are dropped.
--
-- >>> equalFilePath "foo" "foo"
-- True
-- >>> equalFilePath "foo" "foo/"
-- True
-- >>> equalFilePath "foo" "./foo"
-- True
-- >>> equalFilePath "" ""
-- True
-- >>> equalFilePath "foo" "/foo"
-- False
-- >>> equalFilePath "foo" "FOO"
-- False
-- >>> equalFilePath "foo" "../foo"
-- False
--
-- prop> \p -> equalFilePath p p
equalFilePath :: PosixFilePath -> PosixFilePath -> Bool
equalFilePath p1 p2 = f p1 == f p2
  where
    f x = dropTrailingPathSeparator $ normalise x


-- | Check if a path is relative
--
-- prop> \path -> isRelative path /= isAbsolute path
isRelative :: PosixFilePath -> Bool
isRelative = not . isAbsolute


-- | Check if a path is absolute
--
-- >>> isAbsolute "/path"
-- True
-- >>> isAbsolute "path"
-- False
-- >>> isAbsolute ""
-- False
isAbsolute :: PosixFilePath -> Bool
isAbsolute (PFP x)
    | BS.length x > 0 = isPathSeparator (BS.head x)
    | otherwise = False


-- | Is a FilePath valid, i.e. could you create a file like it?
--
-- >>> isValid ""
-- False
-- >>> isValid "\0"
-- False
-- >>> isValid "/random_ path:*"
-- True
isValid :: PosixFilePath -> Bool
isValid (PFP filepath)
  | BS.null filepath        = False
  | _nul `BS.elem` filepath = False
  | otherwise               = True


-- | Take a FilePath and make it valid; does not change already valid FilePaths.
--
-- >>> makeValid ""
-- PFP "_"
-- >>> makeValid "file\0name"
-- PFP "file_name"
--
-- prop> \p -> if isValid p then makeValid p == p else makeValid p /= p
-- prop> \p -> isValid (makeValid p)
makeValid :: PosixFilePath -> PosixFilePath
makeValid (PFP path)
  | BS.null path = PFP $ BS.singleton _underscore
  | otherwise    = PFP $ BS.map (\x -> if x == _nul then _underscore else x) path


-- | Whether the filename is a special directory entry
-- (. and ..). Does not normalise filepaths.
--
-- >>> isSpecialDirectoryEntry "."
-- True
-- >>> isSpecialDirectoryEntry ".."
-- True
-- >>> isSpecialDirectoryEntry "/random_ path:*"
-- False
isSpecialDirectoryEntry :: PosixFilePath -> Bool
isSpecialDirectoryEntry (PFP filepath)
  | BS.pack [_period, _period] == filepath = True
  | BS.pack [_period] == filepath          = True
  | otherwise                              = False


-- | Is the given path a valid filename? This includes
-- "." and "..".
--
-- >>> isFileName "lal"
-- True
-- >>> isFileName "."
-- True
-- >>> isFileName ".."
-- True
-- >>> isFileName ""
-- False
-- >>> isFileName "\0"
-- False
-- >>> isFileName "/random_ path:*"
-- False
isFileName :: PosixFilePath -> Bool
isFileName (PFP filepath) =
  not (foldr (\a b -> BS.singleton a `BS.isInfixOf` filepath && b) True pathSeparators) &&
  not (BS.null filepath) &&
  not (_nul `BS.elem` filepath)


-- | Check if the filepath has any parent directories in it.
--
-- >>> hasParentDir "/.."
-- True
-- >>> hasParentDir "foo/bar/.."
-- True
-- >>> hasParentDir "foo/../bar/."
-- True
-- >>> hasParentDir "foo/bar"
-- False
-- >>> hasParentDir "foo"
-- False
-- >>> hasParentDir ""
-- False
-- >>> hasParentDir ".."
-- False
hasParentDir :: PosixFilePath -> Bool
hasParentDir (PFP filepath) =
    predicate (`BS.cons` pathDoubleDot)
     BS.isSuffixOf
   ||
    predicate (\sep -> BS.singleton sep
        `BS.append` pathDoubleDot
        `BS.append` BS.singleton sep)
     BS.isInfixOf
   ||
    predicate (\sep -> BS.snoc sep pathDoubleDot )
      BS.isPrefixOf
  where
    pathDoubleDot = BS.pack [_period, _period]
    predicate f p =
      foldr (\a b -> f a
              `p` filepath && b)
            True
            pathSeparators


-- | Whether the file is a hidden file.
--
-- >>> hiddenFile ".foo"
-- True
-- >>> hiddenFile "..foo.bar"
-- True
-- >>> hiddenFile "some/path/.bar"
-- True
-- >>> hiddenFile "..."
-- True
-- >>> hiddenFile "dod.bar"
-- False
-- >>> hiddenFile "."
-- False
-- >>> hiddenFile ".."
-- False
-- >>> hiddenFile ""
-- False
hiddenFile :: PosixFilePath -> Bool
hiddenFile fp
  | fn == BS.pack [_period, _period] = False
  | fn == BS.pack [_period]          = False
  | otherwise                        = BS.pack [extSeparator]
                                         `BS.isPrefixOf` fn
  where
    (PFP fn) = takeFileName fp



------------------------
-- internal stuff

-- Just split the input FileName without adding/normalizing or changing
-- anything.
splitFileNameRaw :: PosixFilePath -> (PosixFilePath, PosixFilePath)
splitFileNameRaw (PFP fp) = (\(x, y) -> (PFP x, PFP y)) $ BS.breakEnd isPathSeparator fp

-- | Combine two paths, assuming rhs is NOT absolute.
combineRaw :: PosixFilePath -> PosixFilePath -> PosixFilePath
combineRaw (PFP a) (PFP b) | BS.null a = PFP b
                           | BS.null b = PFP a
                           | isPathSeparator (BS.last a) = PFP $ BS.append a b
                           | otherwise = PFP $ BS.intercalate (BS.singleton pathSeparator) [a, b]
