{-# LANGUAGE CPP #-}


module AbstractFilePath.Windows
  (
  -- * Separator predicates
    pathSeparator
  , pathSeparators
  , isPathSeparator
  , searchPathSeparator
  , isSearchPathSeparator
  , extSeparator
  , isExtSeparator

  -- * $PATH methods
  , splitSearchPath

  -- * Extension functions
  , takeExtension
  , replaceExtension
  , dropExtension
  , addExtension
  , hasExtension
  , (<.>)
  , splitExtensions
  , dropExtensions
  , takeExtensions
  , splitExtension

  -- * Filename\/directory functions
  , splitFileName
  , takeFileName
  , replaceFileName
  , dropFileName
  , takeBaseName
  , replaceBaseName
  , takeDirectory
  , replaceDirectory
  , combine
  , (</>)
  , splitPath
  , joinPath
  , splitDirectories
  , takeAllParents

  -- * Drive functions
  , splitDrive
  , joinDrive
  , takeDrive
  , hasDrive
  , dropDrive
  , isDrive

  -- * Trailing slash functions
  , hasTrailingPathSeparator
  , addTrailingPathSeparator
  , dropTrailingPathSeparator

  -- * File name manipulations
  , normalise
  , makeRelative
  , equalFilePath
  , isRelative
  , isAbsolute
  , isValid
  , makeValid
  , isFileName
  , hasParentDir
  )
where


import AbstractFilePath.Internal.Windows
  ( pathSeparator
  , pathSeparators
  , isPathSeparator
  , searchPathSeparator
  , isSearchPathSeparator
  , extSeparator
  , isExtSeparator
  )
import AbstractFilePath.Internal.Types ( WindowsString(..), WindowsFilePath )

import Control.Arrow (second)
import Data.Bifunctor (bimap, first)
import Data.ByteString (ByteString)
import Data.Maybe (isJust)
import Data.Word8
    ( Word8, _colon, _nul, _period, _slash, _underscore )
import Data.ByteString.Short (ShortByteString)

import qualified AbstractFilePath.Internal.Windows as C


-- $setup
-- >>> import Data.Char
-- >>> import Data.Maybe
-- >>> import Data.Word8
-- >>> import Test.QuickCheck
-- >>> import Control.Applicative
-- >>> import AbstractFilePath.Internal.Types (WindowsString (..))
-- >>> import qualified AbstractFilePath.ShortByteString.Word16 as BS
-- >>> instance Arbitrary ShortByteString where arbitrary = sized $ \n -> choose (0,n) >>= \k -> fmap BS.pack $ vectorOf (if even k then k else k + 1) arbitrary
-- >>> instance Arbitrary WindowsString where arbitrary = WFP <$> arbitrary
--
-- >>> let _chr :: Word8 -> Char; _chr = chr . fromIntegral




------------------------
-- $PATH methods


-- | Take a WindowsString, split it on the 'searchPathSeparator'.
-- Blank items are converted to @.@.
--
-- >>> splitSearchPath "File1;File2;File3"
-- ["File1","File2","File3"]
-- >>> splitSearchPath "File1;;File2;File3"
-- ["File1","File2","File3"]
-- >>> splitSearchPath "File1;\"File2\";File3"
-- ["File1","File2","File3"]
-- >>> splitSearchPath ""
-- []
splitSearchPath :: WindowsString -> [WindowsFilePath]
splitSearchPath (WFP s) = fmap WFP . C.splitSearchPath $ s



------------------------
-- Extension functions

-- | Split a 'WindowsFilePath' into a path+filename and extension
--
-- >>> splitExtension "file.exe"
-- ("file",".exe")
-- >>> splitExtension "file"
-- ("file","")
-- >>> splitExtension "/path/file.tar.gz"
-- ("/path/file.tar",".gz")
-- >>> splitExtension ".exe"
-- (".exe","")
--
-- prop> \path -> WFP (uncurry (\a b -> BS.append (unWFP a) (unWFP b)) (splitExtension path)) == path
splitExtension :: WindowsFilePath -> (WindowsFilePath, WindowsString)
splitExtension (WFP x) = bimap WFP WFP $ C.splitExtension x


-- | Get the file extension from a 'WindowsFilePath'
--
-- >>> takeExtension "file.exe"
-- ".exe"
-- >>> takeExtension "file"
-- ""
-- >>> takeExtension "/path/file.tar.gz"
-- ".gz"
takeExtension :: WindowsFilePath -> WindowsString
takeExtension (WFP x) = WFP $ C.takeExtension x


-- | Change a file's extension
--
-- prop> \path -> let ext = takeExtension path in replaceExtension path ext == path
replaceExtension :: WindowsFilePath -> WindowsString -> WindowsFilePath
replaceExtension (WFP path) (WFP ext) = WFP (C.replaceExtension path ext)


-- | Drop the final extension from a 'WindowsFilePath'
--
-- >>> dropExtension ".exe"
-- ".exe"
-- >>> dropExtension "file.exe"
-- "file"
-- >>> dropExtension "file"
-- "file"
-- >>> dropExtension "/path/file.tar.gz"
-- "/path/file.tar"
dropExtension :: WindowsFilePath -> WindowsFilePath
dropExtension (WFP x) = WFP $ C.dropExtension x


-- | Add an extension to a 'WindowsFilePath'
--
-- >>> addExtension "file" ".exe"
-- "file.exe"
-- >>> addExtension "file.tar" ".gz"
-- "file.tar.gz"
-- >>> addExtension "/path/" ".ext"
-- "/path/.ext"
addExtension :: WindowsFilePath -> WindowsString -> WindowsFilePath
addExtension (WFP bs) (WFP ext) = WFP $ C.addExtension bs ext


-- | Check if a 'WindowsFilePath' has an extension
--
-- >>> hasExtension "file"
-- False
-- >>> hasExtension "file.tar"
-- True
-- >>> hasExtension "/path.part1/"
-- False
hasExtension :: WindowsFilePath -> Bool
hasExtension (WFP x) = C.hasExtension x


-- | Operator version of 'addExtension'
(<.>) :: WindowsFilePath -> WindowsString -> WindowsFilePath
(<.>) = addExtension


-- | Split a 'WindowsFilePath' on the first extension.
--
-- >>> splitExtensions "/path/file.tar.gz"
-- ("/path/file",".tar.gz")
--
-- prop> \path -> uncurry addExtension (splitExtensions path) == path
splitExtensions :: WindowsFilePath -> (WindowsFilePath, WindowsString)
splitExtensions (WFP x) = bimap WFP WFP $  C.splitExtensions x


-- | Remove all extensions from a 'WindowsFilePath'
--
-- >>> dropExtensions "/path/file.tar.gz"
-- "/path/file"
dropExtensions :: WindowsFilePath -> WindowsFilePath
dropExtensions (WFP x) = WFP $ C.dropExtensions x


-- | Take all extensions from a 'WindowsFilePath'
--
-- >>> takeExtensions "/path/file.tar.gz"
-- ".tar.gz"
takeExtensions :: WindowsFilePath -> WindowsString
takeExtensions (WFP x) = WFP $ C.takeExtensions x


-- | Drop the given extension from a FilePath, and the @\".\"@ preceding it.
-- Returns 'Nothing' if the FilePath does not have the given extension, or
-- 'Just' and the part before the extension if it does.
--
-- This function can be more predictable than 'dropExtensions',
-- especially if the filename might itself contain @.@ characters.
--
-- >>> stripExtension "hs.o" "foo.x.hs.o"
-- Just "foo.x"
-- >>> stripExtension "hi.o" "foo.x.hs.o"
-- Nothing
-- >>> stripExtension ".c.d" "a.b.c.d"
-- Just "a.b"
-- >>> stripExtension ".c.d" "a.b..c.d"
-- Just "a.b."
-- >>> stripExtension "baz"  "foo.bar"
-- Nothing
-- >>> stripExtension "bar"  "foobar"
-- Nothing
--
-- prop> \path -> stripExtension "" path == Just path
-- prop> \path -> dropExtension path  == fromJust (stripExtension (takeExtension path) path)
-- prop> \path -> dropExtensions path == fromJust (stripExtension (takeExtensions path) path)
stripExtension :: WindowsString -> WindowsFilePath -> Maybe WindowsFilePath
stripExtension (WFP bs) (WFP x) = fmap WFP $ C.stripExtension bs x


------------------------
-- Filename/directory functions


-- | Split a 'WindowsFilePath' into (path,file).  'combine' is the inverse
--
-- >>> splitFileName "path\\file.txt"
-- ("path\","file.txt")
-- >>> splitFileName "path\\"
-- ("path\","")
-- >>> splitFileName "file.txt"
-- (".\","file.txt")
--
-- prop> \path -> uncurry combine (splitFileName path) == path || fst (splitFileName path) == ".\\"
splitFileName :: WindowsFilePath -> (WindowsFilePath, WindowsFilePath)
splitFileName (WFP x) = bimap WFP WFP $ C.splitFileName x


-- | Get the file name
--
-- >>> takeFileName "path/file.txt"
-- "file.txt"
-- >>> takeFileName "path/"
-- ""
takeFileName :: WindowsFilePath -> WindowsFilePath
takeFileName (WFP x) = WFP $ C.takeFileName x


-- | Change the file name
--
-- prop> \path -> let fn = takeFileName path in replaceFileName path fn == path
replaceFileName :: WindowsFilePath -> WindowsString -> WindowsFilePath
replaceFileName (WFP x) (WFP y) = WFP $ C.replaceFileName x y


-- | Drop the file name
--
-- >>> dropFileName "path\\file.txt"
-- "path\"
-- >>> dropFileName "file.txt"
-- ".\"
dropFileName :: WindowsFilePath -> WindowsFilePath
dropFileName (WFP x) = WFP $ C.dropFileName x


-- | Get the file name, without a trailing extension
--
-- >>> takeBaseName "path/file.tar.gz"
-- "file.tar"
-- >>> takeBaseName ""
-- ""
takeBaseName :: WindowsFilePath -> WindowsFilePath
takeBaseName (WFP x) = WFP $ C.takeBaseName x


-- | Change the base name
--
-- >>> replaceBaseName "path/file.tar.gz" "bob"
-- "path/bob.gz"
--
-- prop> \path -> let baseName = takeBaseName path in replaceBaseName path baseName == path
replaceBaseName :: WindowsFilePath -> WindowsString -> WindowsFilePath
replaceBaseName (WFP path) (WFP name) = WFP $ C.replaceBaseName path name


-- | Get the directory, moving up one level if it's already a directory
--
-- >>> takeDirectory "path/file.txt"
-- "path"
-- >>> takeDirectory "file"
-- "."
-- >>> takeDirectory "/path/to/"
-- "/path/to"
-- >>> takeDirectory "/path/to"
-- "/path"
takeDirectory :: WindowsFilePath -> WindowsFilePath
takeDirectory (WFP x) = WFP $ C.takeDirectory x


-- | Change the directory component of a 'WindowsFilePath'
--
-- prop> \path -> replaceDirectory path (takeDirectory path) `equalFilePath` path || takeDirectory path == "."
replaceDirectory :: WindowsFilePath -> WindowsFilePath -> WindowsFilePath
replaceDirectory (WFP file) (WFP dir) = WFP $ C.replaceDirectory file dir


-- | Join two paths together. If the second path is absolute, then returns it, ignoring
--  the first path.
--
-- >>> combine "/" "file"
-- "/file"
-- >>> combine "/path/to" "file"
-- "/path/to/file"
-- >>> combine "file" "/absolute/path"
-- "/absolute/path"
combine :: WindowsFilePath -> WindowsFilePath -> WindowsFilePath
combine (WFP a) (WFP b) = WFP $ C.combine a b


-- | Operator version of combine
(</>) :: WindowsFilePath -> WindowsFilePath -> WindowsFilePath
(</>) = combine

-- | Split a path into a list of components:
--
-- >>> splitPath "/path/to/file.txt"
-- ["/","path/","to/","file.txt"]
--
-- prop> \path -> WFP (BS.concat (fmap (\(WFP fp) -> fp) (splitPath path))) == path
splitPath :: WindowsFilePath -> [WindowsFilePath]
splitPath (WFP bs) = fmap WFP $ C.splitPath bs


-- | Join a split path back together
--
-- prop> \path -> joinPath (splitPath path) == path
--
-- >>> joinPath ["path","to","file.txt"]
-- WFP "path/to/file.txt"
joinPath :: [WindowsFilePath] -> WindowsFilePath
joinPath = foldr (</>) (WFP mempty)


-- | Like 'splitPath', but without trailing slashes
--
-- >>> splitDirectories "/path/to/file.txt"
-- ["/","path","to","file.txt"]
-- >>> splitDirectories "path/to/file.txt"
-- ["path","to","file.txt"]
-- >>> splitDirectories "/"
-- ["/"]
-- >>> splitDirectories ""
-- []
splitDirectories :: WindowsFilePath -> [WindowsFilePath]
splitDirectories (WFP x) = fmap WFP $ C.splitDirectories x


-- |Get all parents of a path.
--
-- >>> takeAllParents "/abs/def/dod"
-- ["/abs/def","/abs","/"]
-- >>> takeAllParents "/foo"
-- ["/"]
-- >>> takeAllParents "/"
-- []
takeAllParents :: WindowsFilePath -> [WindowsFilePath]
takeAllParents (WFP p) = fmap WFP $ C.takeAllParents p


------------------------
-- Drive functions

-- | Split a path into a drive and a path.
--   On Posix, \/ is a Drive.
--
-- >>> splitDrive "/test"
-- ("/","test")
-- >>> splitDrive "//test"
-- ("//","test")
-- >>> splitDrive "test/file"
-- ("","test/file")
-- >>> splitDrive "file"
-- ("","file")
--
-- prop> \x -> uncurry (<>) (splitDrive x) == x
splitDrive :: WindowsFilePath -> (WindowsFilePath, WindowsFilePath)
splitDrive (WFP p) = bimap WFP WFP $ C.splitDrive p


-- | Join a drive and the rest of the path.
--
-- prop> \x -> uncurry joinDrive (splitDrive x) == x
joinDrive :: WindowsFilePath -> WindowsFilePath -> WindowsFilePath
joinDrive (WFP a) (WFP b) = WFP $ C.joinDrive a b


-- | Get the drive from a filepath.
--
-- prop> \x -> takeDrive x == fst (splitDrive x)
takeDrive :: WindowsFilePath -> WindowsFilePath
takeDrive (WFP x) = WFP $ C.takeDrive x


-- | Does a path have a drive.
--
-- >>> hasDrive "/foo"
-- True
-- >>> hasDrive "foo"
-- False
--
-- prop> \x -> not (hasDrive x) == BS.null ((\(WFP x) -> x) $ takeDrive x)
hasDrive :: WindowsFilePath -> Bool
hasDrive (WFP x) = C.hasDrive x


-- | Delete the drive, if it exists.
--
-- prop> \x -> dropDrive x == snd (splitDrive x)
dropDrive :: WindowsFilePath -> WindowsFilePath
dropDrive (WFP x) = WFP $ C.dropDrive x


-- | Is an element a drive
--
-- >>> isDrive "/"
-- True
-- >>> isDrive "/foo"
-- False
isDrive :: WindowsFilePath -> Bool
isDrive (WFP x) = C.isDrive x


------------------------
-- Trailing slash functions

-- | Check if the last character of a 'WindowsFilePath' is '/'.
--
-- >>> hasTrailingPathSeparator "/path/"
-- True
-- >>> hasTrailingPathSeparator "/"
-- True
-- >>> hasTrailingPathSeparator "/path"
-- False
hasTrailingPathSeparator :: WindowsFilePath -> Bool
hasTrailingPathSeparator (WFP x) = C.hasTrailingPathSeparator x


-- | Add a trailing path separator.
--
-- >>> addTrailingPathSeparator "/path"
-- "/path/"
-- >>> addTrailingPathSeparator "/path/"
-- "/path/"
-- >>> addTrailingPathSeparator "/"
-- "/"
addTrailingPathSeparator :: WindowsFilePath -> WindowsFilePath
addTrailingPathSeparator (WFP bs) = WFP $ C.addTrailingPathSeparator bs


-- | Remove a trailing path separator
--
-- >>> dropTrailingPathSeparator "/path/"
-- "/path"
-- >>> dropTrailingPathSeparator "/path////"
-- "/path"
-- >>> dropTrailingPathSeparator "/"
-- "/"
-- >>> dropTrailingPathSeparator "//"
-- "/"
dropTrailingPathSeparator :: WindowsFilePath -> WindowsFilePath
dropTrailingPathSeparator (WFP x) = WFP $ C.dropTrailingPathSeparator x



------------------------
-- File name manipulations


-- |Normalise a file.
--
-- >>> normalise "/file/\\test////"
-- "/file/\\test/"
-- >>> normalise "/file/./test"
-- "/file/test"
-- >>> normalise "/test/file/../bob/fred/"
-- "/test/file/../bob/fred/"
-- >>> normalise "../bob/fred/"
-- "../bob/fred/"
-- >>> normalise "./bob/fred/"
-- "bob/fred/"
-- >>> normalise "./bob////.fred/./...///./..///#."
-- "bob/.fred/.../../#."
-- >>> normalise "."
-- "."
-- >>> normalise "./"
-- "./"
-- >>> normalise "./."
-- "./"
-- >>> normalise "/./"
-- "/"
-- >>> normalise "/"
-- "/"
-- >>> normalise "bob/fred/."
-- "bob/fred/"
-- >>> normalise "//home"
-- "/home"
normalise :: WindowsFilePath -> WindowsFilePath
normalise (WFP filepath) = WFP $ C.normalise filepath


-- | Contract a filename, based on a relative path. Note that the resulting
-- path will never introduce @..@ paths, as the presence of symlinks
-- means @..\/b@ may not reach @a\/b@ if it starts from @a\/c@. For a
-- worked example see
-- <http://neilmitchell.blogspot.co.uk/2015/10/filepaths-are-subtle-symlinks-are-hard.html this blog post>.
--
-- >>> makeRelative "/directory" "/directory/file.ext"
-- "file.ext"
-- >>> makeRelative "/Home" "/home/bob"
-- "/home/bob"
-- >>> makeRelative "/home/" "/home/bob/foo/bar"
-- "bob/foo/bar"
-- >>> makeRelative "/fred" "bob"
-- "bob"
-- >>> makeRelative "/file/test" "/file/test/fred"
-- "fred"
-- >>> makeRelative "/file/test" "/file/test/fred/"
-- "fred/"
-- >>> makeRelative "some/path" "some/path/a/b/c"
-- "a/b/c"
--
-- prop> \p -> makeRelative p p == "."
-- prop> \p -> makeRelative (takeDirectory p) p `equalFilePath` takeFileName p
-- prop \x y -> equalFilePath x y || (isRelative x && makeRelative y x == x) || equalFilePath (y </> makeRelative y x) x
makeRelative :: WindowsFilePath -> WindowsFilePath -> WindowsFilePath
makeRelative (WFP root) (WFP path) = WFP $ C.makeRelative root path


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
equalFilePath :: WindowsFilePath -> WindowsFilePath -> Bool
equalFilePath (WFP p1) (WFP p2) = C.equalFilePath p1 p2


-- | Check if a path is relative
--
-- prop> \path -> isRelative path /= isAbsolute path
isRelative :: WindowsFilePath -> Bool
isRelative (WFP x) = C.isRelative x


-- | Check if a path is absolute
--
-- >>> isAbsolute "/path"
-- True
-- >>> isAbsolute "path"
-- False
-- >>> isAbsolute ""
-- False
isAbsolute :: WindowsFilePath -> Bool
isAbsolute (WFP x) = C.isAbsolute x


-- | Is a FilePath valid, i.e. could you create a file like it?
--
-- >>> isValid ""
-- False
-- >>> isValid "\0"
-- False
-- >>> isValid "/random_ path:*"
-- True
isValid :: WindowsFilePath -> Bool
isValid (WFP filepath) = C.isValid filepath


-- | Take a FilePath and make it valid; does not change already valid FilePaths.
--
-- >>> makeValid ""
-- "_"
-- >>> makeValid "file\0name"
-- "file_name"
--
-- prop> \p -> if isValid p then makeValid p == p else makeValid p /= p
-- prop> \p -> isValid (makeValid p)
makeValid :: WindowsFilePath -> WindowsFilePath
makeValid (WFP path) = WFP $ C.makeValid path


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
isFileName :: WindowsFilePath -> Bool
isFileName (WFP filepath) = C.isFileName filepath


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
hasParentDir :: WindowsFilePath -> Bool
hasParentDir (WFP filepath) = C.hasParentDir filepath


