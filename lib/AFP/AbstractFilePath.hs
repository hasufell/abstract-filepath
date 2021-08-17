{-# LANGUAGE CPP #-}

-- |
-- Module      :  AbstractFilePath
-- Copyright   :  © 2021 Julian Ospald
-- License     :  MIT
--
-- Maintainer  :  Julian Ospald <hasufell@posteo.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- An implementation of the <https://gitlab.haskell.org/ghc/ghc/-/wikis/proposal/abstract-file-path Abstract FilePath Proposal>,
-- which aims to supersede @type FilePath = String@ for various reasons:
--
-- 1. it is more efficient (uses unpinned 'ShortByteString' under the hood)
-- 2. is more type-safe (not a type synonym, but a newtype)
-- 3. avoids round-tripping issues, by not converting to String (which loses the encoding)
--
-- It is important to know that filenames\/filepaths have different representations across platforms:
--
-- - On /Windows/, filepaths are expected to be in UTF16 as passed to
--   syscalls (although there are other APIs, the <https://hackage.haskell.org/package/Win32 Win32> package uses the wide character one).
--   This invariant is maintained by 'AbstractFilePath'.
-- - On /Unix/, filepaths don't have a predefined encoding (although they
--   are often interpreted as UTF8) as per the
--   <https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap03.html#tag_03_170 POSIX specification>
--   and are passed as @char[]@ to syscalls. 'AbstractFilePath' maintains no invariant
--   here. Some functions however, such as 'toAbstractFilePath', may expect
--   or produce UTF8.
--
-- Apart from encoding, filepaths have additional restrictions per platform:
--
-- - On /Windows/ the <https://docs.microsoft.com/en-us/windows/win32/fileio/naming-a-file#naming-conventions naming convention> may apply
-- - On /Unix/, only @NUL@ bytes are disallowed as per the <https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap03.html#tag_03_170 POSIX specification>
--
-- Use 'filePathIsValid' to check for these restrictions ('AbstractFilePath' doesn't
-- maintain this invariant).
--
-- Also note that these restrictions are
-- not exhaustive and further filesystem specific restrictions may apply on
-- all platforms. This library makes no attempt at satisfying these.
-- Library users may need to account for that, depending
-- on what filesystems they want to support.
--
-- It is advised to follow these principles when dealing with filepaths\/filenames:
--
-- 1. Avoid interpreting filenames that the OS returns, unless absolutely necessary.
--    For example, the filepath separator is usually a predefined 'Word8', regardless of encoding.
--    So even if we need to split filepaths, it might still not be necessary to understand the encoding
--    of the filename.
-- 2. When interpreting OS returned filenames consider that these might not be UTF8 on /unix/
--    or at worst don't have an ASCII compatible encoding. Some strategies here involve looking
--    up the current locale and using that for decoding ('fromAbstractFilePathIO' does this).
--    Otherwise it can be reasonable to assume UTF8 on unix ('fromAbstractFilePath' does that) if your application specifically
--    mentions that it requires a UTF8 compatible system. These things should be documented.
-- 3. When dealing with user input (e.g. on the command line) on /unix/ as e.g. @String@ the input
--    encoding is lost. The output encoding (e.g. how we write a filename to disk) can then
--    either follow the current locale again ('toAbstractFilePath'') or a fixed encoding
--    ('toAbstractFilePath'). The decision should be clearly documented. If the input is in the
--    form of a @ByteString@, then 'bsToAFP' may be of interest, unless the input needs further
--    interpretation.

module AFP.AbstractFilePath
  (
  -- * Types
    AbstractFilePath
  , OsString
  , OsWord

  -- * Construction
  , toAbstractFilePath
  , toAbstractFilePathIO
  , bsToAFP
  , afp
  , packAFP

  -- * Deconstruction
  , fromAbstractFilePath
  , fromAbstractFilePathIO
  , unpackAFP

  -- * Word construction
  , fromChar

  -- * Separator predicates
  , pathSeparator
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

import AFP.AbstractFilePath.Internal
    ( afp
    , bsToAFP
    , fromAbstractFilePath
    , fromAbstractFilePathIO
    , toAbstractFilePath
    , toAbstractFilePathIO
    , unpackAFP
    , packAFP
    )
import AFP.AbstractFilePath.Internal.Types
    ( AbstractFilePath, PosixFilePath, WindowsFilePath )
import AFP.OsString
import AFP.OsString.Internal ( fromChar )
import AFP.OsString.Internal.Types

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import qualified AFP.AbstractFilePath.Windows as AFP
#else
import qualified AFP.AbstractFilePath.Posix as AFP
#endif

import Data.Bifunctor
    ( bimap, first )


------------------------
-- Separator predicates


-- | Ideal path separator character
pathSeparator :: OsWord
pathSeparator = OsWord AFP.pathSeparator

-- | All path separator characters
pathSeparators :: [OsWord]
pathSeparators = OsWord <$> AFP.pathSeparators

-- | Check if a character is the path separator
--
-- > (n == '/') == isPathSeparator n
isPathSeparator :: OsWord -> Bool
isPathSeparator (OsWord w) = AFP.isPathSeparator w

-- | Search path separator
searchPathSeparator :: OsWord
searchPathSeparator = OsWord AFP.searchPathSeparator

-- | Check if a character is the search path separator
--
-- > (n == ':') == isSearchPathSeparator n
isSearchPathSeparator :: OsWord -> Bool
isSearchPathSeparator (OsWord fp) = AFP.isSearchPathSeparator fp


-- | File extension separator
extSeparator :: OsWord
extSeparator = OsWord AFP.extSeparator

-- | Check if a character is the file extension separator
--
-- > (n == '.') == isExtSeparator n
isExtSeparator :: OsWord -> Bool
isExtSeparator (OsWord fp) = AFP.isExtSeparator fp


------------------------
-- $PATH methods


-- | Take an 'OsString', split it on the 'searchPathSeparator'.
-- On Windows path elements are stripped of quotes.
-- On Posix blank items are converted to @.@.
--
-- Follows the recommendations in
-- <http://www.opengroup.org/onlinepubs/009695399/basedefs/xbd_chap08.html>
--
-- > Windows: splitSearchPath "File1;File2;File3" == ["File1","File2","File3"]
-- > Windows: splitSearchPath "File1;;File2;File3" == ["File1","File2","File3"]
-- > Windows: splitSearchPath "File1;\"File2\";File3" == ["File1","File2","File3"]
-- > Windows: splitSearchPath "" == []
-- > Posix: splitSearchPath "File1:File2:File3" == ["File1","File2","File3"]
-- > Posix: splitSearchPath "File1::File2:File3" == ["File1",".","File2","File3"]
-- > Posix: splitSearchPath "" == ["."]
splitSearchPath :: OsString -> [AbstractFilePath]
splitSearchPath (OsString str) = fmap OsString . AFP.splitSearchPath $ str



------------------------
-- Extension functions

-- | Split a path into a path+filename and extension
--
-- > splitExtension "file.exe" == ("file",".exe")
-- > splitExtension "file" == ("file","")
-- > splitExtension "/path/file.tar.gz" == ("/path/file.tar",".gz")
-- > splitExtension ".exe" == (".exe","")
--
-- > (\(a, b) -> a <> b) (splitExtension path)) == path
splitExtension :: AbstractFilePath -> (AbstractFilePath, OsString)
splitExtension (OsString x) = bimap OsString OsString $ AFP.splitExtension x


-- | Get the file extension from a path
--
-- > takeExtension "file.exe" == ".exe"
-- > takeExtension "file" == ""
-- > takeExtension "/path/file.tar.gz" == ".gz"
takeExtension :: AbstractFilePath -> OsString
takeExtension (OsString x) = OsString $ AFP.takeExtension x


-- | Change a file's extension
--
-- > replaceExtension path (takeExtension path) == path
replaceExtension :: AbstractFilePath -> OsString -> AbstractFilePath
replaceExtension (OsString path) (OsString ext) = OsString (AFP.replaceExtension path ext)


-- | Drop the final extension from a path
--
-- > dropExtension ".exe" == ".exe"
-- > dropExtension "file.exe" == "file"
-- > dropExtension "file" = "file"
-- > dropExtension "/path/file.tar.gz" = "/path/file.tar"
dropExtension :: AbstractFilePath -> AbstractFilePath
dropExtension (OsString x) = OsString $ AFP.dropExtension x


-- | Add an extension to a path
--
-- > addExtension "file" ".exe" == "file.exe"
-- > addExtension "file.tar" ".gz" == "file.tar.gz"
-- > addExtension "/path/" ".ext" == "/path/.ext"
addExtension :: AbstractFilePath -> OsString -> AbstractFilePath
addExtension (OsString bs) (OsString ext) = OsString $ AFP.addExtension bs ext


-- | Check if a path has an extension
--
-- > hasExtension "file" == False
-- > hasExtension "file.tar" == True
-- > hasExtension "/path.part1/" == False
hasExtension :: AbstractFilePath -> Bool
hasExtension (OsString x) = AFP.hasExtension x


-- | Operator version of 'addExtension'
(<.>) :: AbstractFilePath -> OsString -> AbstractFilePath
(<.>) = addExtension


-- | Split a path on the first extension.
--
-- > splitExtensions "/path/file.tar.gz" == ("/path/file",".tar.gz")
-- > uncurry addExtension (splitExtensions path) == path
splitExtensions :: AbstractFilePath -> (AbstractFilePath, OsString)
splitExtensions (OsString x) = bimap OsString OsString $  AFP.splitExtensions x


-- | Remove all extensions from a path
--
-- > dropExtensions "/path/file.tar.gz" == "/path/file"
dropExtensions :: AbstractFilePath -> AbstractFilePath
dropExtensions (OsString x) = OsString $ AFP.dropExtensions x


-- | Take all extensions from a path
--
-- > takeExtensions "/path/file.tar.gz" == ".tar.gz"
takeExtensions :: AbstractFilePath -> OsString
takeExtensions (OsString x) = OsString $ AFP.takeExtensions x


-- | Drop the given extension from a path, and the @\".\"@ preceding it.
-- Returns 'Nothing' if the path does not have the given extension, or
-- 'Just' and the part before the extension if it does.
--
-- This function can be more predictable than 'dropExtensions',
-- especially if the filename might itself contain @.@ characters.
--
-- > stripExtension "hs.o" "foo.x.hs.o" == Just "foo.x"
-- > stripExtension "hi.o" "foo.x.hs.o" == Nothing
-- > stripExtension ".c.d" "a.b.c.d" == Just "a.b"
-- > stripExtension ".c.d" "a.b..c.d" == Just "a.b."
-- > stripExtension "baz"  "foo.bar" == Nothing
-- > stripExtension "bar"  "foobar" == Nothing
-- > stripExtension "" path == Just path
-- > dropExtension path  == fromJust (stripExtension (takeExtension path) path)
-- > dropExtensions path == fromJust (stripExtension (takeExtensions path) path)
stripExtension :: OsString -> AbstractFilePath -> Maybe AbstractFilePath
stripExtension (OsString bs) (OsString x) = fmap OsString $ AFP.stripExtension bs x


------------------------
-- Filename/directory functions


-- | Split a path into (path,file).  'combine' is the inverse
--
-- > Windows: splitFileName "path\\file.txt" == ("path\","file.txt")
-- > Windows: splitFileName "path\\" == ("path\","")
-- > Windows: splitFileName "file.txt" == (".\","file.txt")
-- > Posix: splitFileName "path/file.txt" == ("path/","file.txt")
-- > Posix: splitFileName "path/" == ("path/","")
-- > Posix: splitFileName "file.txt" == ("./","file.txt")
-- > uncurry combine (splitFileName path) == path || fst (splitFileName path) == "./"
splitFileName :: AbstractFilePath -> (AbstractFilePath, AbstractFilePath)
splitFileName (OsString x) = bimap OsString OsString $ AFP.splitFileName x


-- | Get the file name
--
-- > takeFileName "path/file.txt" == "file.txt"
-- > takeFileName "path/" == ""
takeFileName :: AbstractFilePath -> AbstractFilePath
takeFileName (OsString x) = OsString $ AFP.takeFileName x


-- | Change the file name
--
-- > replaceFileName path (takeFileName path) == path
replaceFileName :: AbstractFilePath -> OsString -> AbstractFilePath
replaceFileName (OsString x) (OsString y) = OsString $ AFP.replaceFileName x y


-- | Drop the file name
--
-- > Windows: dropFileName "path\\file.txt" == "path\"
-- > Windows: dropFileName "file.txt" == ".\"
-- > Posix: dropFileName "path/file.txt" == "path/"
-- > Posix: dropFileName "file.txt" == "./"
dropFileName :: AbstractFilePath -> AbstractFilePath
dropFileName (OsString x) = OsString $ AFP.dropFileName x


-- | Get the file name, without a trailing extension
--
-- > takeBaseName "path/file.tar.gz" == "file.tar"
-- > takeBaseName "" == ""
takeBaseName :: AbstractFilePath -> AbstractFilePath
takeBaseName (OsString x) = OsString $ AFP.takeBaseName x


-- | Change the base name
--
-- > replaceBaseName "path/file.tar.gz" "bob" == "path/bob.gz"
-- > replaceBaseName path (takeBaseName path) == path
replaceBaseName :: AbstractFilePath -> OsString -> AbstractFilePath
replaceBaseName (OsString path) (OsString name) = OsString $ AFP.replaceBaseName path name


-- | Get the directory, moving up one level if it's already a directory
--
-- > takeDirectory "path/file.txt" == "path"
-- > takeDirectory "file" == "."
-- > takeDirectory "/path/to/" == "/path/to"
-- > takeDirectory "/path/to" == "/path"
takeDirectory :: AbstractFilePath -> AbstractFilePath
takeDirectory (OsString x) = OsString $ AFP.takeDirectory x


-- | Change the directory component of a path
--
-- > replaceDirectory path (takeDirectory path) `equalFilePath` path || takeDirectory path == "."
replaceDirectory :: AbstractFilePath -> AbstractFilePath -> AbstractFilePath
replaceDirectory (OsString file) (OsString dir) = OsString $ AFP.replaceDirectory file dir


-- | Join two paths together. If the second path is absolute, then returns it, ignoring
--  the first path.
--
-- > Windows: combine "C:\\" "file" == "C:\file"
-- > Windows: combine "C:\\path\\to" "file" == "C:\path\to\file"
-- > Windows: combine "file" "\\absolute\\path" == "\absolute\path"
-- > Posix: combine "/" "file" == "/file"
-- > Posix: combine "/path/to" "file" == "/path/to/file"
-- > Posix: combine "file" "/absolute/path" == "/absolute/path"
combine :: AbstractFilePath -> AbstractFilePath -> AbstractFilePath
combine (OsString a) (OsString b) = OsString $ AFP.combine a b


-- | Operator version of 'combine'
(</>) :: AbstractFilePath -> AbstractFilePath -> AbstractFilePath
(</>) = combine

-- | Split a path into a list of components:
--
-- > splitPath "/path/to/file.txt" == ["/","path/","to/","file.txt"]
-- > mconcat (splitPath path) == path
splitPath :: AbstractFilePath -> [AbstractFilePath]
splitPath (OsString bs) = fmap OsString $ AFP.splitPath bs


-- | Join a split path back together
--
-- > joinPath (splitPath path) == path
-- > Windows: joinPath ["path","to","file.txt"] == "path\to\file.txt"
-- > Posix: joinPath ["path","to","file.txt"] == "path/to/file.txt"
joinPath :: [AbstractFilePath] -> AbstractFilePath
joinPath = foldr (</>) (OsString mempty)


-- | Like 'splitPath', but without trailing slashes
--
-- > splitDirectories "/path/to/file.txt" == ["/","path","to","file.txt"]
-- > splitDirectories "path/to/file.txt" == ["path","to","file.txt"]
-- > splitDirectories "/" == ["/"]
-- > splitDirectories "" == []
splitDirectories :: AbstractFilePath -> [AbstractFilePath]
splitDirectories (OsString x) = fmap OsString $ AFP.splitDirectories x


-- |Get all parents of a path.
--
-- > Windows: takeAllParents "C:\\foo\\bar" == ["C:\foo","C:"]
-- > Windows: takeAllParents "/abs/def/dod" == ["\abs\def","\abs","\"]
-- > Windows: takeAllParents "/foo" == ["\"]
-- > Posix: takeAllParents "/abs/def/dod" == ["/abs/def","/abs","/"]
-- > Posix: takeAllParents "/foo" == ["/"]
-- > takeAllParents "/" == []
takeAllParents :: AbstractFilePath -> [AbstractFilePath]
takeAllParents (OsString p) = fmap OsString $ AFP.takeAllParents p


------------------------
-- Drive functions

-- | Split a path into a drive and a path.
--   On Posix, \/ is a Drive.
--
-- > Windows: splitDrive "/test" == ("","/test")
-- > Windows: splitDrive "C:\\file" == ("C:\","file")
-- > Posix: splitDrive "/test" == ("/","test")
-- > splitDrive "//test" == ("//","test")
-- > splitDrive "test/file" == ("","test/file")
-- > splitDrive "file" == ("","file")
-- > uncurry (<>) (splitDrive x) == x
splitDrive :: AbstractFilePath -> (AbstractFilePath, AbstractFilePath)
splitDrive (OsString p) = bimap OsString OsString $ AFP.splitDrive p


-- | Join a drive and the rest of the path.
--
-- > uncurry joinDrive (splitDrive x) == x
joinDrive :: AbstractFilePath -> AbstractFilePath -> AbstractFilePath
joinDrive (OsString a) (OsString b) = OsString $ AFP.joinDrive a b


-- | Get the drive from a filepath.
--
-- > takeDrive x == fst (splitDrive x)
takeDrive :: AbstractFilePath -> AbstractFilePath
takeDrive (OsString x) = OsString $ AFP.takeDrive x


-- | Does a path have a drive.
--
-- > Windows: hasDrive "C:\\foo" == True
-- > Windows: hasDrive "/foo" == False
-- > Posix: hasDrive "/foo" == True
-- > hasDrive "foo" == False
-- > not (hasDrive x) == null (takeDrive x)
hasDrive :: AbstractFilePath -> Bool
hasDrive (OsString x) = AFP.hasDrive x


-- | Delete the drive, if it exists.
--
-- > dropDrive x == snd (splitDrive x)
dropDrive :: AbstractFilePath -> AbstractFilePath
dropDrive (OsString x) = OsString $ AFP.dropDrive x


-- | Is an element a drive
--
-- > Windows: isDrive "C:" == True
-- > Windows: isDrive "/" == False
-- > Posix: isDrive "/" == True
-- > isDrive "/foo" == False
isDrive :: AbstractFilePath -> Bool
isDrive (OsString x) = AFP.isDrive x


------------------------
-- Trailing slash functions

-- | Check if the last character of a path is '/'.
--
-- > hasTrailingPathSeparator "/path/" == True
-- > hasTrailingPathSeparator "/" == True
-- > hasTrailingPathSeparator "/path" == False
hasTrailingPathSeparator :: AbstractFilePath -> Bool
hasTrailingPathSeparator (OsString x) = AFP.hasTrailingPathSeparator x


-- | Add a trailing path separator.
--
-- > Windows: addTrailingPathSeparator "/path" == "/path\"
-- > Posix: addTrailingPathSeparator "/path" == "/path/"
-- > addTrailingPathSeparator "/path/" == "/path/"
-- > addTrailingPathSeparator "/" == "/"
addTrailingPathSeparator :: AbstractFilePath -> AbstractFilePath
addTrailingPathSeparator (OsString bs) = OsString $ AFP.addTrailingPathSeparator bs


-- | Remove trailing path separators
--
-- > dropTrailingPathSeparator "/path/" == "/path"
-- > dropTrailingPathSeparator "/path////" == "/path"
-- > dropTrailingPathSeparator "/" == "/"
-- > Posix: dropTrailingPathSeparator "//" == "/"
-- > Windows: dropTrailingPathSeparator "//" == "//"
dropTrailingPathSeparator :: AbstractFilePath -> AbstractFilePath
dropTrailingPathSeparator (OsString x) = OsString $ AFP.dropTrailingPathSeparator x



------------------------
-- File name manipulations


-- |Normalise a file path.
--
-- > Posix: normalise "/file/\\test////" == "/file/\test/"
-- > Windows: normalise "/file/\\test////" == "\file\test\"
-- > normalise "/file/./test" == "/file/test"
-- > normalise "/test/file/../bob/fred/" == "/test/file/../bob/fred/"
-- > normalise "../bob/fred/" == "../bob/fred/"
-- > normalise "./bob/fred/" == "bob/fred/"
-- > normalise "./bob////.fred/./...///./..///#." == "bob/.fred/.../../#."
-- > normalise "." == "."
-- > normalise "./" == "./"
-- > normalise "./." == "./"
-- > normalise "/./" == "/"
-- > normalise "/" == "/"
-- > normalise "bob/fred/." == "bob/fred/"
-- > normalise "//home" == "/home"
normalise :: AbstractFilePath -> AbstractFilePath
normalise (OsString filepath) = OsString $ AFP.normalise filepath


-- | Contract a filename, based on a relative path. Note that the resulting
-- path will never introduce @..@ paths, as the presence of symlinks
-- means @..\/b@ may not reach @a\/b@ if it starts from @a\/c@. For a
-- worked example see
-- <http://neilmitchell.blogspot.co.uk/2015/10/filepaths-are-subtle-symlinks-are-hard.html this blog post>.
--
-- > Posix: makeRelative "/Home" "/home/bob" == "/home/bob"
-- > Windows: makeRelative "/Home" "/home/bob" == "bob"
-- > makeRelative "/directory" "/directory/file.ext" == "file.ext"
-- > makeRelative "/home/" "/home/bob/foo/bar" == "bob/foo/bar"
-- > makeRelative "/fred" "bob" == "bob"
-- > makeRelative "/file/test" "/file/test/fred" == "fred"
-- > makeRelative "/file/test" "/file/test/fred/" == "fred/"
-- > makeRelative "some/path" "some/path/a/b/c" == "a/b/c"
-- > makeRelative p p == "."
-- > makeRelative (takeDirectory p) p `equalFilePath` takeFileName p
-- > equalFilePath x y || (isRelative x && makeRelative y x == x) || equalFilePath (y </> makeRelative y x) x
makeRelative :: AbstractFilePath -> AbstractFilePath -> AbstractFilePath
makeRelative (OsString root) (OsString path) = OsString $ AFP.makeRelative root path


-- |Equality of two filepaths. The filepaths are normalised
-- and trailing path separators are dropped.
--
-- > Posix: equalFilePath "foo" "FOO" == False
-- > Windows: equalFilePath "foo" "FOO" == False
-- > equalFilePath "foo" "foo" == True
-- > equalFilePath "foo" "foo/" == True
-- > equalFilePath "foo" "./foo" == True
-- > equalFilePath "" "" == True
-- > equalFilePath "foo" "/foo" == False
-- > equalFilePath "foo" "../foo" == False
-- > equalFilePath p p == True
equalFilePath :: AbstractFilePath -> AbstractFilePath -> Bool
equalFilePath (OsString p1) (OsString p2) = AFP.equalFilePath p1 p2


-- | Check if a path is relative
--
-- > isRelative path /= isAbsolute path
isRelative :: AbstractFilePath -> Bool
isRelative (OsString x) = AFP.isRelative x


-- | Check if a path is absolute
--
-- > Windows: isAbsolute "C:\\path" == True
-- > Windows: isAbsolute "/path" == False
-- > Posix: isAbsolute "/path" == True
-- > isAbsolute "path" == False
-- > isAbsolute "" == False
isAbsolute :: AbstractFilePath -> Bool
isAbsolute (OsString x) = AFP.isAbsolute x


-- | Is a FilePath valid, i.e. could you create a file like it?
--
-- > isValid "" == False
-- > isValid "\0" == False
-- > isValid "/random_path" == True
isValid :: AbstractFilePath -> Bool
isValid (OsString filepath) = AFP.isValid filepath


-- | Take a FilePath and make it valid; does not change already valid FilePaths.
--
-- > makeValid "" == "_"
-- > makeValid "file\0name" == "file_name"
-- > if isValid p then makeValid p == p else makeValid p /= p
-- > isValid (makeValid p)
makeValid :: AbstractFilePath -> AbstractFilePath
makeValid (OsString path) = OsString $ AFP.makeValid path


#ifndef WINDOWS
-- | Whether the filename is a special directory entry
-- (. and ..). Does not normalise filepaths.
--
-- This is only defined for POSIX.
--
-- > isSpecialDirectoryEntry "." == True
-- > isSpecialDirectoryEntry ".." == True
-- > isSpecialDirectoryEntry "/random_ path:*" == False
isSpecialDirectoryEntry :: AbstractFilePath -> Bool
isSpecialDirectoryEntry (OsString filepath) = AFP.isSpecialDirectoryEntry filepath
#endif

-- | Is the given path a valid filename? This includes
-- "." and "..".
--
-- > Posix: isFileName "/random_path" == False
-- > Windows: isFileName "/random_path" == True
-- > isFileName "lal" == True
-- > isFileName "." == True
-- > isFileName ".." == True
-- > isFileName "" == False
-- > isFileName "\0" == False
isFileName :: AbstractFilePath -> Bool
isFileName (OsString filepath) = AFP.isFileName filepath


-- | Check if the filepath has any parent directories in it.
--
-- > hasParentDir "/.." == True
-- > hasParentDir "foo/bar/.." == True
-- > hasParentDir "foo/../bar/." == True
-- > hasParentDir "foo/bar" == False
-- > hasParentDir "foo" == False
-- > hasParentDir "" == False
-- > hasParentDir ".." == False
hasParentDir :: AbstractFilePath -> Bool
hasParentDir (OsString filepath) = AFP.hasParentDir filepath


#ifndef WINDOWS
-- | Whether the file is a hidden file.
--
-- This is only defined on POSIX.
--
-- > hiddenFile ".foo" == True
-- > hiddenFile "..foo.bar" == True
-- > hiddenFile "some/path/.bar" == True
-- > hiddenFile "..." == True
-- > hiddenFile "dod.bar" == False
-- > hiddenFile "." == False
-- > hiddenFile ".." == False
-- > hiddenFile "" == False
hiddenFile :: AbstractFilePath -> Bool
hiddenFile (OsString fp) = AFP.hiddenFile fp
#endif
