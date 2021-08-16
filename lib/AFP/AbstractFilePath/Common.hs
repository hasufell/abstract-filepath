{-# LANGUAGE CPP #-}
-- This template expects CPP definitions for:
--     MODULE_NAME   = Posix | Windows
--     IS_WINDOWS    = False | True
--     FILEPATH_NAME = PosixFilePath | WindowsFilePath
--     CTOR          = PS | WS

module AFP.AbstractFilePath.MODULE_NAME
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
  , stripExtension

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

#ifndef WINDOWS
  -- * posix specific functions
  , hiddenFile
  , isSpecialDirectoryEntry
#endif
  )
where

-- doctest
import AFP.AbstractFilePath.Internal.Types
    ()

import qualified AFP.AbstractFilePath.Internal.MODULE_NAME as IP
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

import qualified AFP.AbstractFilePath.Internal.MODULE_NAME as C
#ifdef WINDOWS
import qualified AFP.Data.ByteString.Short.Word16 as BS
#else
import qualified AFP.Data.ByteString.Short as BS
#endif


#ifdef WINDOWS
-- $setup
-- >>> import Data.Char
-- >>> import Data.Maybe
-- >>> import Data.Word8
-- >>> import Test.QuickCheck
-- >>> import Control.Applicative
-- >>> import AFP.AbstractFilePath.Internal.Types
-- >>> import AFP.Data.ByteString.Short as BS (concat)
-- >>> import qualified AFP.Data.ByteString.Short.Word16 as BS
-- >>> instance Arbitrary ShortByteString where arbitrary = BS.pack <$> arbitrary
-- >>> instance CoArbitrary ShortByteString where coarbitrary = coarbitrary . BS.unpack
-- >>> instance Arbitrary WindowsFilePath where arbitrary = WS <$> arbitrary
-- >>> instance CoArbitrary WindowsFilePath where coarbitrary = coarbitrary . (\(WS fp) -> fp)
-- >>> import AFP.OsString.Internal.Types (WindowsString (..))
-- >>> instance Arbitrary ShortByteString where arbitrary = sized $ \n -> choose (0,n) >>= \k -> fmap BS.pack $ vectorOf (if even k then k else k + 1) arbitrary
-- >>> instance Arbitrary WindowsString where arbitrary = WS <$> arbitrary
-- >>> let _chr :: Word -> Char; _chr = chr . fromIntegral
#else
-- $setup
-- >>> import Data.Char
-- >>> import Data.Maybe
-- >>> import Data.Word8
-- >>> import Test.QuickCheck
-- >>> import Control.Applicative
-- >>> import AFP.AbstractFilePath.Internal.Types
-- >>> import qualified Data.ByteString.Short as BS
-- >>> instance Arbitrary ShortByteString where arbitrary = BS.pack <$> arbitrary
-- >>> instance CoArbitrary ShortByteString where coarbitrary = coarbitrary . BS.unpack
-- >>> instance Arbitrary PosixFilePath where arbitrary = PS <$> arbitrary
-- >>> instance CoArbitrary PosixFilePath where coarbitrary = coarbitrary . (\(PS fp) -> fp)
-- >>> import AFP.OsString.Internal.Types (PosixString (..))
-- >>> instance Arbitrary ShortByteString where arbitrary = sized $ \n -> choose (0,n) >>= \k -> fmap BS.pack $ vectorOf (if even k then k else k + 1) arbitrary
-- >>> instance Arbitrary PosixString where arbitrary = PS <$> arbitrary
-- >>> let _chr :: Word -> Char; _chr = chr . fromIntegral
#endif


------------------------
-- Separator predicates


-- | Ideal path separator character
pathSeparator :: WORD_NAME
pathSeparator = WTOR IP.pathSeparator

-- | All path separator characters
pathSeparators :: [WORD_NAME]
pathSeparators = WTOR <$> IP.pathSeparators

-- | Check if a character is the path separator
--
-- >  (n == '/') == isPathSeparator n
isPathSeparator :: WORD_NAME -> Bool
isPathSeparator (WTOR w) = IP.isPathSeparator w

-- | Search path separator
searchPathSeparator :: WORD_NAME
searchPathSeparator = WTOR IP.searchPathSeparator

-- | Check if a character is the search path separator
--
-- > (n == ':') == isSearchPathSeparator n
isSearchPathSeparator :: WORD_NAME -> Bool
isSearchPathSeparator (WTOR w) = IP.isSearchPathSeparator w


-- | File extension separator
extSeparator :: WORD_NAME
extSeparator = WTOR IP.extSeparator


-- | Check if a character is the file extension separator
--
-- > (n == '.') == isExtSeparator n
isExtSeparator :: WORD_NAME -> Bool
isExtSeparator (WTOR w) = IP.isExtSeparator w


------------------------
-- $PATH methods


#ifdef WINDOWS
-- | Take a string, split it on the 'searchPathSeparator'.
-- Path elements are stripped of quotes.
--
-- >>> splitSearchPath "File1;File2;File3"
-- ["File1","File2","File3"]
-- >>> splitSearchPath "File1;;File2;File3"
-- ["File1","File2","File3"]
-- >>> splitSearchPath "File1;\"File2\";File3"
-- ["File1","File2","File3"]
-- >>> splitSearchPath ""
-- []
#else
-- | Take a string, split it on the 'searchPathSeparator'.
-- Blank items are converted to @.@.
--
-- Follows the recommendations in
-- <http://www.opengroup.org/onlinepubs/009695399/basedefs/xbd_chap08.html>
--
-- >>> splitSearchPath "File1:File2:File3"
-- ["File1","File2","File3"]
-- >>> splitSearchPath "File1::File2:File3"
-- ["File1",".","File2","File3"]
-- >>> splitSearchPath ""
-- ["."]
#endif
splitSearchPath :: OSSTRING_NAME -> [FILEPATH_NAME]
splitSearchPath (CTOR x) = fmap CTOR . C.splitSearchPath $ x



------------------------
-- Extension functions

-- | Split a path into a path+filename and extension
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
-- prop> \path -> CTOR (uncurry (\(CTOR a) (CTOR b) -> BS.append a b) (splitExtension path)) == path
splitExtension :: FILEPATH_NAME -> (FILEPATH_NAME, OSSTRING_NAME)
splitExtension (CTOR x) = bimap CTOR CTOR $ C.splitExtension x


-- | Get the file extension from a path
--
-- >>> takeExtension "file.exe"
-- ".exe"
-- >>> takeExtension "file"
-- ""
-- >>> takeExtension "/path/file.tar.gz"
-- ".gz"
takeExtension :: FILEPATH_NAME -> OSSTRING_NAME
takeExtension (CTOR x) = CTOR $ C.takeExtension x


-- | Change a file's extension
--
-- prop> \path -> let ext = takeExtension path in replaceExtension path ext == path
replaceExtension :: FILEPATH_NAME -> OSSTRING_NAME -> FILEPATH_NAME
replaceExtension (CTOR path) (CTOR ext) = CTOR (C.replaceExtension path ext)


-- | Drop the final extension from a path
--
-- >>> dropExtension ".exe"
-- ".exe"
-- >>> dropExtension "file.exe"
-- "file"
-- >>> dropExtension "file"
-- "file"
-- >>> dropExtension "/path/file.tar.gz"
-- "/path/file.tar"
dropExtension :: FILEPATH_NAME -> FILEPATH_NAME
dropExtension (CTOR x) = CTOR $ C.dropExtension x


-- | Add an extension to a path
--
-- >>> addExtension "file" ".exe"
-- "file.exe"
-- >>> addExtension "file.tar" ".gz"
-- "file.tar.gz"
-- >>> addExtension "/path/" ".ext"
-- "/path/.ext"
addExtension :: FILEPATH_NAME -> OSSTRING_NAME -> FILEPATH_NAME
addExtension (CTOR bs) (CTOR ext) = CTOR $ C.addExtension bs ext


-- | Check if a path has an extension
--
-- >>> hasExtension "file"
-- False
-- >>> hasExtension "file.tar"
-- True
-- >>> hasExtension "/path.part1/"
-- False
hasExtension :: FILEPATH_NAME -> Bool
hasExtension (CTOR x) = C.hasExtension x


-- | Operator version of 'addExtension'
(<.>) :: FILEPATH_NAME -> OSSTRING_NAME -> FILEPATH_NAME
(<.>) = addExtension


-- | Split a path on the first extension.
--
-- >>> splitExtensions "/path/file.tar.gz"
-- ("/path/file",".tar.gz")
--
-- prop> \path -> uncurry addExtension (splitExtensions path) == path
splitExtensions :: FILEPATH_NAME -> (FILEPATH_NAME, OSSTRING_NAME)
splitExtensions (CTOR x) = bimap CTOR CTOR $ C.splitExtensions x


-- | Remove all extensions from a path
--
-- >>> dropExtensions "/path/file.tar.gz"
-- "/path/file"
dropExtensions :: FILEPATH_NAME -> FILEPATH_NAME
dropExtensions (CTOR x) = CTOR $ C.dropExtensions x


-- | Take all extensions from a path
--
-- >>> takeExtensions "/path/file.tar.gz"
-- ".tar.gz"
takeExtensions :: FILEPATH_NAME -> OSSTRING_NAME
takeExtensions (CTOR x) = CTOR $ C.takeExtensions x


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
stripExtension :: OSSTRING_NAME -> FILEPATH_NAME -> Maybe FILEPATH_NAME
stripExtension (CTOR bs) (CTOR x) = fmap CTOR $ C.stripExtension bs x


------------------------
-- Filename/directory functions


#ifdef WINDOWS
-- | Split a path into (path,file).  'combine' is the inverse
--
-- >>> splitFileName "path\\file.txt"
-- ("path\","file.txt")
-- >>> splitFileName "path\\"
-- ("path\","")
-- >>> splitFileName "file.txt"
-- (".\","file.txt")
--
-- prop> \path -> uncurry combine (splitFileName path) == path || fst (splitFileName path) == ".\\"
#else
-- | Split a path into (path,file).  'combine' is the inverse
--
-- >>> splitFileName "path/file.txt"
-- ("path/","file.txt")
-- >>> splitFileName "path/"
-- ("path/","")
-- >>> splitFileName "file.txt"
-- ("./","file.txt")
--
-- prop> \path -> uncurry combine (splitFileName path) == path || fst (splitFileName path) == "./"
#endif
splitFileName :: FILEPATH_NAME -> (FILEPATH_NAME, FILEPATH_NAME)
splitFileName (CTOR x) = bimap CTOR CTOR $ C.splitFileName x


-- | Get the file name
--
-- >>> takeFileName "path/file.txt"
-- "file.txt"
-- >>> takeFileName "path/"
-- ""
takeFileName :: FILEPATH_NAME -> FILEPATH_NAME
takeFileName (CTOR x) = CTOR $ C.takeFileName x


-- | Change the file name
--
-- prop> \path -> let fn = takeFileName path in replaceFileName path fn == path
replaceFileName :: FILEPATH_NAME -> OSSTRING_NAME -> FILEPATH_NAME
replaceFileName (CTOR x) (CTOR y) = CTOR $ C.replaceFileName x y


#ifdef WINDOWS
-- | Drop the file name
--
-- >>> dropFileName "path\\file.txt"
-- "path\"
-- >>> dropFileName "file.txt"
-- ".\"
#else
-- | Drop the file name
--
-- >>> dropFileName "path/file.txt"
-- "path/"
-- >>> dropFileName "file.txt"
-- "./"
#endif
dropFileName :: FILEPATH_NAME -> FILEPATH_NAME
dropFileName (CTOR x) = CTOR $ C.dropFileName x


-- | Get the file name, without a trailing extension
--
-- >>> takeBaseName "path/file.tar.gz"
-- "file.tar"
-- >>> takeBaseName ""
-- ""
takeBaseName :: FILEPATH_NAME -> FILEPATH_NAME
takeBaseName (CTOR x) = CTOR $ C.takeBaseName x


-- | Change the base name
--
-- >>> replaceBaseName "path/file.tar.gz" "bob"
-- "path/bob.gz"
--
-- prop> \path -> let baseName = takeBaseName path in replaceBaseName path baseName == path
replaceBaseName :: FILEPATH_NAME -> OSSTRING_NAME -> FILEPATH_NAME
replaceBaseName (CTOR path) (CTOR name) = CTOR $ C.replaceBaseName path name


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
takeDirectory :: FILEPATH_NAME -> FILEPATH_NAME
takeDirectory (CTOR x) = CTOR $ C.takeDirectory x


-- | Change the directory component of a path
--
-- prop> \path -> replaceDirectory path (takeDirectory path) `equalFilePath` path || takeDirectory path == "."
replaceDirectory :: FILEPATH_NAME -> FILEPATH_NAME -> FILEPATH_NAME
replaceDirectory (CTOR file) (CTOR dir) = CTOR $ C.replaceDirectory file dir


#ifdef WINDOWS
-- | Join two paths together. If the second path is absolute, then returns it, ignoring
--  the first path.
--
-- >>> combine "C:\\" "file"
-- "C:\file"
-- >>> combine "C:\\path\\to" "file"
-- "C:\path\to\file"
-- >>> combine "file" "\\absolute\\path"
-- "\absolute\path"
#else
-- | Join two paths together. If the second path is absolute, then returns it, ignoring
--  the first path.
--
-- >>> combine "/" "file"
-- "/file"
-- >>> combine "/path/to" "file"
-- "/path/to/file"
-- >>> combine "file" "/absolute/path"
-- "/absolute/path"
#endif
combine :: FILEPATH_NAME -> FILEPATH_NAME -> FILEPATH_NAME
combine (CTOR a) (CTOR b) = CTOR $ C.combine a b


-- | Operator version of combine
(</>) :: FILEPATH_NAME -> FILEPATH_NAME -> FILEPATH_NAME
(</>) = combine

-- | Split a path into a list of components:
--
-- >>> splitPath "/path/to/file.txt"
-- ["/","path/","to/","file.txt"]
--
-- prop> \path -> CTOR (BS.concat (fmap (\(CTOR fp) -> fp) (splitPath path))) == path
splitPath :: FILEPATH_NAME -> [FILEPATH_NAME]
splitPath (CTOR bs) = fmap CTOR $ C.splitPath bs


#ifdef WINDOWS
-- | Join a split path back together
--
-- prop> \path -> joinPath (splitPath path) == path
--
-- >>> joinPath ["path","to","file.txt"]
-- "path\to\file.txt"
#else
-- | Join a split path back together
--
-- prop> \path -> joinPath (splitPath path) == path
--
-- >>> joinPath ["path","to","file.txt"]
-- "path/to/file.txt"
#endif
joinPath :: [FILEPATH_NAME] -> FILEPATH_NAME
joinPath = foldr (</>) (CTOR mempty)


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
splitDirectories :: FILEPATH_NAME -> [FILEPATH_NAME]
splitDirectories (CTOR x) = fmap CTOR $ C.splitDirectories x


#ifdef WINDOWS
-- |Get all parents of a path.
--
-- >>> takeAllParents "C:\\foo\\bar"
-- ["C:\\foo","C:\"]
-- >>> takeAllParents "/abs/def/dod"
-- ["\\abs\def","\\abs\","\\"]
-- >>> takeAllParents "/foo"
-- ["\"]
-- >>> takeAllParents "/"
-- []
#else
-- |Get all parents of a path.
--
-- >>> takeAllParents "/abs/def/dod"
-- ["/abs/def","/abs","/"]
-- >>> takeAllParents "/foo"
-- ["/"]
-- >>> takeAllParents "/"
-- []
#endif
takeAllParents :: FILEPATH_NAME -> [FILEPATH_NAME]
takeAllParents (CTOR p) = fmap CTOR $ C.takeAllParents p


------------------------
-- Drive functions

#ifdef WINDOWS
-- | Split a path into a drive and a path.
--
-- >>> splitDrive "/test"
-- ("","/test")
-- >>> splitDrive "C:\\file"
-- ("C:\","file")
-- >>> splitDrive "//test"
-- ("//test","")
-- >>> splitDrive "test/file"
-- ("","test/file")
-- >>> splitDrive "file"
-- ("","file")
--
-- prop> \x -> uncurry (<>) (splitDrive x) == x
#else
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
#endif
splitDrive :: FILEPATH_NAME -> (FILEPATH_NAME, FILEPATH_NAME)
splitDrive (CTOR p) = bimap CTOR CTOR $ C.splitDrive p


-- | Join a drive and the rest of the path.
--
-- prop> \x -> uncurry joinDrive (splitDrive x) == x
joinDrive :: FILEPATH_NAME -> FILEPATH_NAME -> FILEPATH_NAME
joinDrive (CTOR a) (CTOR b) = CTOR $ C.joinDrive a b


-- | Get the drive from a filepath.
--
-- prop> \x -> takeDrive x == fst (splitDrive x)
takeDrive :: FILEPATH_NAME -> FILEPATH_NAME
takeDrive (CTOR x) = CTOR $ C.takeDrive x


#ifdef WINDOWS
-- | Does a path have a drive.
--
-- >>> hasDrive "C:\\foo"
-- True
-- >>> hasDrive "/foo"
-- False
-- >>> hasDrive "foo"
-- False
--
-- prop> \x -> not (hasDrive x) == BS.null ((\(CTOR x) -> x) $ takeDrive x)
#else
-- | Does a path have a drive.
--
-- >>> hasDrive "/foo"
-- True
-- >>> hasDrive "foo"
-- False
--
-- prop> \x -> not (hasDrive x) == BS.null ((\(CTOR x) -> x) $ takeDrive x)
#endif
hasDrive :: FILEPATH_NAME -> Bool
hasDrive (CTOR x) = C.hasDrive x


-- | Delete the drive, if it exists.
--
-- prop> \x -> dropDrive x == snd (splitDrive x)
dropDrive :: FILEPATH_NAME -> FILEPATH_NAME
dropDrive (CTOR x) = CTOR $ C.dropDrive x


#ifdef WINDOWS
-- | Is an element a drive
--
-- >>> isDrive "C:"
-- True
-- >>> isDrive "/"
-- False
-- >>> isDrive "/foo"
-- False
#else
-- | Is an element a drive
--
-- >>> isDrive "/"
-- True
-- >>> isDrive "/foo"
-- False
#endif
isDrive :: FILEPATH_NAME -> Bool
isDrive (CTOR x) = C.isDrive x


------------------------
-- Trailing slash functions

-- | Check if the last character of a path is '/'.
--
-- >>> hasTrailingPathSeparator "/path/"
-- True
-- >>> hasTrailingPathSeparator "/"
-- True
-- >>> hasTrailingPathSeparator "/path"
-- False
hasTrailingPathSeparator :: FILEPATH_NAME -> Bool
hasTrailingPathSeparator (CTOR x) = C.hasTrailingPathSeparator x


#ifdef WINDOWS
-- | Add a trailing path separator.
--
-- >>> addTrailingPathSeparator "/path"
-- "/path\"
-- >>> addTrailingPathSeparator "/path/"
-- "/path/"
-- >>> addTrailingPathSeparator "/"
-- "/"
#else
-- | Add a trailing path separator.
--
-- >>> addTrailingPathSeparator "/path"
-- "/path/"
-- >>> addTrailingPathSeparator "/path/"
-- "/path/"
-- >>> addTrailingPathSeparator "/"
-- "/"
#endif
addTrailingPathSeparator :: FILEPATH_NAME -> FILEPATH_NAME
addTrailingPathSeparator (CTOR bs) = CTOR $ C.addTrailingPathSeparator bs


#ifdef WINDOWS
-- | Remove a trailing path separator
--
-- >>> dropTrailingPathSeparator "/path/"
-- "/path"
-- >>> dropTrailingPathSeparator "/path////"
-- "/path"
-- >>> dropTrailingPathSeparator "/"
-- "/"
-- >>> dropTrailingPathSeparator "//"
-- "//"
#else
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
#endif
dropTrailingPathSeparator :: FILEPATH_NAME -> FILEPATH_NAME
dropTrailingPathSeparator (CTOR x) = CTOR $ C.dropTrailingPathSeparator x



------------------------
-- File name manipulations


#ifdef WINDOWS
-- |Normalise a file.
--
-- >>> normalise "/file/\\test////"
-- "\file\test\"
-- >>> normalise "/file/./test"
-- "\file\test"
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
#else
-- |Normalise a file.
--
-- >>> normalise "/file/\\test////"
-- "/file/\test/"
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
#endif
normalise :: FILEPATH_NAME -> FILEPATH_NAME
normalise (CTOR filepath) = CTOR $ C.normalise filepath


#ifdef WINDOWS
-- | Contract a filename, based on a relative path. Note that the resulting
-- path will never introduce @..@ paths, as the presence of symlinks
-- means @..\/b@ may not reach @a\/b@ if it starts from @a\/c@. For a
-- worked example see
-- <http://neilmitchell.blogspot.co.uk/2015/10/filepaths-are-subtle-symlinks-are-hard.html this blog post>.
--
-- >>> makeRelative "/directory" "/directory/file.ext"
-- "file.ext"
-- >>> makeRelative "/Home" "/home/bob"
-- "bob"
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
#else
-- | Contract a filename, based on a relative path. Note that the resulting
-- path will never introduce @..@ paths, as the presence of symlinks
-- means @..\/b@ may not reach @a\/b@ if it starts from @a\/c@. For a
-- worked example see
-- <http://neilmitchell.blogspot.co.uk/2015/10/filepaths-are-subtle-symlinks-are-hard.html this blog post>.
--
-- >>> makeRelative "/directory" "/directory/file.ext"
-- "file.ext"
-- >>> makeRelative "/Home" "/home/bob"
-- "bob"
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
#endif
makeRelative :: FILEPATH_NAME -> FILEPATH_NAME -> FILEPATH_NAME
makeRelative (CTOR root) (CTOR path) = CTOR $ C.makeRelative root path


#ifdef WINDOWS
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
-- True
-- >>> equalFilePath "foo" "../foo"
-- False
--
-- prop> \p -> equalFilePath p p
#else
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
#endif
equalFilePath :: FILEPATH_NAME -> FILEPATH_NAME -> Bool
equalFilePath (CTOR p1) (CTOR p2) = C.equalFilePath p1 p2


-- | Check if a path is relative
--
-- prop> \path -> isRelative path /= isAbsolute path
isRelative :: FILEPATH_NAME -> Bool
isRelative (CTOR x) = C.isRelative x


#ifdef WINDOWS
-- | Check if a path is absolute
--
-- >>> isAbsolute "C:\\path"
-- True
-- >>> isAbsolute "/path"
-- False
-- >>> isAbsolute "path"
-- False
-- >>> isAbsolute ""
-- False
#else
-- | Check if a path is absolute
--
-- >>> isAbsolute "/path"
-- True
-- >>> isAbsolute "path"
-- False
-- >>> isAbsolute ""
-- False
#endif
isAbsolute :: FILEPATH_NAME -> Bool
isAbsolute (CTOR x) = C.isAbsolute x


-- | Is a FilePath valid, i.e. could you create a file like it?
--
-- >>> isValid ""
-- False
-- >>> isValid "\0"
-- False
-- >>> isValid "/random_path"
-- True
isValid :: FILEPATH_NAME -> Bool
isValid (CTOR filepath) = C.isValid filepath


-- | Take a FilePath and make it valid; does not change already valid FilePaths.
--
-- >>> makeValid ""
-- "_"
-- >>> makeValid "file\0name"
-- "file_name"
--
-- prop> \p -> if isValid p then makeValid p == p else makeValid p /= p
-- prop> \p -> isValid (makeValid p)
makeValid :: FILEPATH_NAME -> FILEPATH_NAME
makeValid (CTOR path) = CTOR $ C.makeValid path


#ifndef WINDOWS
-- | Whether the filename is a special directory entry
-- (. and ..). Does not normalise filepaths.
--
-- This is only defined for POSIX.
--
-- >>> isSpecialDirectoryEntry "."
-- True
-- >>> isSpecialDirectoryEntry ".."
-- True
-- >>> isSpecialDirectoryEntry "/random_ path:*"
-- False
isSpecialDirectoryEntry :: FILEPATH_NAME -> Bool
isSpecialDirectoryEntry (CTOR filepath)
  | BS.pack [_period, _period] == filepath = True
  | BS.pack [_period] == filepath          = True
  | otherwise                              = False
#endif

#ifndef WINDOWS
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
-- >>> isFileName "/random_path"
-- True
#else
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
-- >>> isFileName "/random_path"
-- False
#endif
isFileName :: FILEPATH_NAME -> Bool
isFileName (CTOR filepath) = C.isFileName filepath


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
hasParentDir :: FILEPATH_NAME -> Bool
hasParentDir (CTOR filepath) = C.hasParentDir filepath


#ifndef WINDOWS
-- | Whether the file is a hidden file.
--
-- This is only defined on POSIX.
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
hiddenFile :: FILEPATH_NAME -> Bool
hiddenFile (CTOR fp)
  | fn == BS.pack [_period, _period] = False
  | fn == BS.pack [_period]          = False
  | otherwise                        = BS.pack [C.extSeparator]
                                         `BS.isPrefixOf` fn
  where
    fn = C.takeFileName fp
#endif
