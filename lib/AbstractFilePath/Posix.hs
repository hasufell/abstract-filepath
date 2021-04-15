{-# LANGUAGE CPP #-}

module AbstractFilePath.Posix
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
 
  -- * posix specific functions
  , hiddenFile
  , isSpecialDirectoryEntry
  )
where


import AbstractFilePath.Internal.Posix
  ( pathSeparator
  , pathSeparators
  , isPathSeparator
  , searchPathSeparator
  , isSearchPathSeparator
  , extSeparator
  , isExtSeparator
  )
import AbstractFilePath.Internal.Types ( PosixString (..), PosixFilePath )

import Control.Arrow (second)
import Data.Bifunctor (bimap, first)
import Data.ByteString (ByteString)
import Data.Maybe (isJust)
import Data.Word8
    ( Word8, _colon, _nul, _period, _slash, _underscore )
import Data.ByteString.Short (ShortByteString)

import qualified Data.ByteString.Short as BS
import qualified AbstractFilePath.ShortByteString as BS
import qualified AbstractFilePath.Internal.Posix as C


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
-- $PATH methods


-- | Take a ShortByteString, split it on the 'searchPathSeparator'.
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
splitSearchPath :: ShortByteString -> [PosixFilePath]
splitSearchPath = fmap PFP . C.splitSearchPath



------------------------
-- Extension functions

-- | Split a 'PosixFilePath' into a path+filename and extension
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
-- prop> \path -> PFP (uncurry (\(PFP a) b -> BS.append a b) (splitExtension path)) == path
splitExtension :: PosixFilePath -> (PosixFilePath, ShortByteString)
splitExtension (PFP x) = first PFP $ C.splitExtension x


-- | Get the file extension from a 'PosixFilePath'
--
-- >>> takeExtension "file.exe"
-- ".exe"
-- >>> takeExtension "file"
-- ""
-- >>> takeExtension "/path/file.tar.gz"
-- ".gz"
takeExtension :: PosixFilePath -> ShortByteString
takeExtension (PFP x) = C.takeExtension x


-- | Change a file's extension
--
-- prop> \path -> let ext = takeExtension path in replaceExtension path ext == path
replaceExtension :: PosixFilePath -> ShortByteString -> PosixFilePath
replaceExtension (PFP path) ext = PFP (C.replaceExtension path ext)


-- | Drop the final extension from a 'PosixFilePath'
--
-- >>> dropExtension ".exe"
-- ".exe"
-- >>> dropExtension "file.exe"
-- "file"
-- >>> dropExtension "file"
-- "file"
-- >>> dropExtension "/path/file.tar.gz"
-- "/path/file.tar"
dropExtension :: PosixFilePath -> PosixFilePath
dropExtension (PFP x) = PFP $ C.dropExtension x


-- | Add an extension to a 'PosixFilePath'
--
-- >>> addExtension "file" ".exe"
-- "file.exe"
-- >>> addExtension "file.tar" ".gz"
-- "file.tar.gz"
-- >>> addExtension "/path/" ".ext"
-- "/path/.ext"
addExtension :: PosixFilePath -> ShortByteString -> PosixFilePath
addExtension (PFP bs) ext = PFP $ C.addExtension bs ext


-- | Check if a 'PosixFilePath' has an extension
--
-- >>> hasExtension "file"
-- False
-- >>> hasExtension "file.tar"
-- True
-- >>> hasExtension "/path.part1/"
-- False
hasExtension :: PosixFilePath -> Bool
hasExtension (PFP x) = C.hasExtension x


-- | Operator version of 'addExtension'
(<.>) :: PosixFilePath -> ShortByteString -> PosixFilePath
(<.>) = addExtension


-- | Split a 'PosixFilePath' on the first extension.
--
-- >>> splitExtensions "/path/file.tar.gz"
-- ("/path/file",".tar.gz")
--
-- prop> \path -> uncurry addExtension (splitExtensions path) == path
splitExtensions :: PosixFilePath -> (PosixFilePath, ShortByteString)
splitExtensions (PFP x) = first PFP $  C.splitExtensions x


-- | Remove all extensions from a 'PosixFilePath'
--
-- >>> dropExtensions "/path/file.tar.gz"
-- "/path/file"
dropExtensions :: PosixFilePath -> PosixFilePath
dropExtensions (PFP x) = PFP $ C.dropExtensions x


-- | Take all extensions from a 'PosixFilePath'
--
-- >>> takeExtensions "/path/file.tar.gz"
-- ".tar.gz"
takeExtensions :: PosixFilePath -> ShortByteString
takeExtensions (PFP x) = C.takeExtensions x


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
stripExtension :: ShortByteString -> PosixFilePath -> Maybe PosixFilePath
stripExtension bs (PFP x) = fmap PFP $ C.stripExtension bs x


------------------------
-- Filename/directory functions


-- | Split a 'PosixFilePath' into (path,file).  'combine' is the inverse
--
-- >>> splitFileName "path/file.txt"
-- ("path/","file.txt")
-- >>> splitFileName "path/"
-- ("path/","")
-- >>> splitFileName "file.txt"
-- ("./","file.txt")
--
-- prop> \path -> uncurry combine (splitFileName path) == path || fst (splitFileName path) == "./"
splitFileName :: PosixFilePath -> (PosixFilePath, PosixFilePath)
splitFileName (PFP x) = bimap PFP PFP $ C.splitFileName x


-- | Get the file name
--
-- >>> takeFileName "path/file.txt"
-- "file.txt"
-- >>> takeFileName "path/"
-- ""
takeFileName :: PosixFilePath -> PosixFilePath
takeFileName (PFP x) = PFP $ C.takeFileName x


-- | Change the file name
--
-- prop> \path -> let (PFP fn) = takeFileName path in replaceFileName path fn == path
replaceFileName :: PosixFilePath -> ShortByteString -> PosixFilePath
replaceFileName (PFP x) y = PFP $ C.replaceFileName x y


-- | Drop the file name
--
-- >>> dropFileName "path/file.txt"
-- "path/"
-- >>> dropFileName "file.txt"
-- "./"
dropFileName :: PosixFilePath -> PosixFilePath
dropFileName (PFP x) = PFP $ C.dropFileName x


-- | Get the file name, without a trailing extension
--
-- >>> takeBaseName "path/file.tar.gz"
-- "file.tar"
-- >>> takeBaseName ""
-- ""
takeBaseName :: PosixFilePath -> PosixFilePath
takeBaseName (PFP x) = PFP $ C.takeBaseName x


-- | Change the base name
--
-- >>> replaceBaseName "path/file.tar.gz" "bob"
-- "path/bob.gz"
--
-- prop> \path -> let (PFP baseName) = takeBaseName path in replaceBaseName path baseName == path
replaceBaseName :: PosixFilePath -> ShortByteString -> PosixFilePath
replaceBaseName (PFP path) name = PFP $ C.replaceBaseName path name


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
takeDirectory :: PosixFilePath -> PosixFilePath
takeDirectory (PFP x) = PFP $ C.takeDirectory x


-- | Change the directory component of a 'PosixFilePath'
--
-- prop> \path -> replaceDirectory path (takeDirectory path) `equalFilePath` path || takeDirectory path == "."
replaceDirectory :: PosixFilePath -> PosixFilePath -> PosixFilePath
replaceDirectory (PFP file) (PFP dir) = PFP $ C.replaceDirectory file dir


-- | Join two paths together. If the second path is absolute, then returns it, ignoring
--  the first path.
--
-- >>> combine "/" "file"
-- "/file"
-- >>> combine "/path/to" "file"
-- "/path/to/file"
-- >>> combine "file" "/absolute/path"
-- "/absolute/path"
combine :: PosixFilePath -> PosixFilePath -> PosixFilePath
combine (PFP a) (PFP b) = PFP $ C.combine a b


-- | Operator version of combine
(</>) :: PosixFilePath -> PosixFilePath -> PosixFilePath
(</>) = combine

-- | Split a path into a list of components:
--
-- >>> splitPath "/path/to/file.txt"
-- ["/","path/","to/","file.txt"]
--
-- prop> \path -> PFP (BS.concat (fmap (\(PFP fp) -> fp) (splitPath path))) == path
splitPath :: PosixFilePath -> [PosixFilePath]
splitPath (PFP bs) = fmap PFP $ C.splitPath bs


-- | Join a split path back together
--
-- prop> \path -> joinPath (splitPath path) == path
--
-- >>> joinPath ["path","to","file.txt"]
-- "path/to/file.txt"
joinPath :: [PosixFilePath] -> PosixFilePath
joinPath = foldr (</>) (PFP mempty)


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
splitDirectories :: PosixFilePath -> [PosixFilePath]
splitDirectories (PFP x) = fmap PFP $ C.splitDirectories x


-- |Get all parents of a path.
--
-- >>> takeAllParents "/abs/def/dod"
-- ["/abs/def","/abs","/"]
-- >>> takeAllParents "/foo"
-- ["/"]
-- >>> takeAllParents "/"
-- []
takeAllParents :: PosixFilePath -> [PosixFilePath]
takeAllParents (PFP p) = fmap PFP $ C.takeAllParents p


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
splitDrive :: PosixFilePath -> (PosixFilePath, PosixFilePath)
splitDrive (PFP p) = bimap PFP PFP $ C.splitDrive p


-- | Join a drive and the rest of the path.
--
-- prop> \x -> uncurry joinDrive (splitDrive x) == x
joinDrive :: PosixFilePath -> PosixFilePath -> PosixFilePath
joinDrive (PFP a) (PFP b) = PFP $ C.joinDrive a b


-- | Get the drive from a filepath.
--
-- prop> \x -> takeDrive x == fst (splitDrive x)
takeDrive :: PosixFilePath -> PosixFilePath
takeDrive (PFP x) = PFP $ C.takeDrive x


-- | Does a path have a drive.
--
-- >>> hasDrive "/foo"
-- True
-- >>> hasDrive "foo"
-- False
--
-- prop> \x -> not (hasDrive x) == BS.null ((\(PFP x) -> x) $ takeDrive x)
hasDrive :: PosixFilePath -> Bool
hasDrive (PFP x) = C.hasDrive x


-- | Delete the drive, if it exists.
--
-- prop> \x -> dropDrive x == snd (splitDrive x)
dropDrive :: PosixFilePath -> PosixFilePath
dropDrive (PFP x) = PFP $ C.dropDrive x


-- | Is an element a drive
--
-- >>> isDrive "/"
-- True
-- >>> isDrive "/foo"
-- False
isDrive :: PosixFilePath -> Bool
isDrive (PFP x) = C.isDrive x


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
hasTrailingPathSeparator (PFP x) = C.hasTrailingPathSeparator x


-- | Add a trailing path separator.
--
-- >>> addTrailingPathSeparator "/path"
-- "/path/"
-- >>> addTrailingPathSeparator "/path/"
-- "/path/"
-- >>> addTrailingPathSeparator "/"
-- "/"
addTrailingPathSeparator :: PosixFilePath -> PosixFilePath
addTrailingPathSeparator (PFP bs) = PFP $ C.addTrailingPathSeparator bs


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
dropTrailingPathSeparator :: PosixFilePath -> PosixFilePath
dropTrailingPathSeparator (PFP x) = PFP $ C.dropTrailingPathSeparator x



------------------------
-- File name manipulations


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
normalise :: PosixFilePath -> PosixFilePath
normalise (PFP filepath) = PFP $ C.normalise filepath


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
makeRelative :: PosixFilePath -> PosixFilePath -> PosixFilePath
makeRelative (PFP root) (PFP path) = PFP $ C.makeRelative root path


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
equalFilePath (PFP p1) (PFP p2) = C.equalFilePath p1 p2


-- | Check if a path is relative
--
-- prop> \path -> isRelative path /= isAbsolute path
isRelative :: PosixFilePath -> Bool
isRelative (PFP x) = C.isRelative x


-- | Check if a path is absolute
--
-- >>> isAbsolute "/path"
-- True
-- >>> isAbsolute "path"
-- False
-- >>> isAbsolute ""
-- False
isAbsolute :: PosixFilePath -> Bool
isAbsolute (PFP x) = C.isAbsolute x


-- | Is a FilePath valid, i.e. could you create a file like it?
--
-- >>> isValid ""
-- False
-- >>> isValid "\0"
-- False
-- >>> isValid "/random_ path:*"
-- True
isValid :: PosixFilePath -> Bool
isValid (PFP filepath) = C.isValid filepath


-- | Take a FilePath and make it valid; does not change already valid FilePaths.
--
-- >>> makeValid ""
-- "_"
-- >>> makeValid "file\0name"
-- "file_name"
--
-- prop> \p -> if isValid p then makeValid p == p else makeValid p /= p
-- prop> \p -> isValid (makeValid p)
makeValid :: PosixFilePath -> PosixFilePath
makeValid (PFP path) = PFP $ C.makeValid path


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
isFileName (PFP filepath) = C.isFileName filepath


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
hasParentDir (PFP filepath) = C.hasParentDir filepath


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
hiddenFile :: PosixFilePath -> Bool
hiddenFile (PFP fp)
  | fn == BS.pack [_period, _period] = False
  | fn == BS.pack [_period]          = False
  | otherwise                        = BS.pack [C.extSeparator]
                                         `BS.isPrefixOf` fn
  where
    fn = C.takeFileName fp

