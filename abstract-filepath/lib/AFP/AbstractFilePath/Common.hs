{-# LANGUAGE CPP #-}
-- This template expects CPP definitions for:
--     WINDOWS
--     POSIX
--     FILEPATH_NAME = PosixFilePath | WindowsFilePath  | AbstractFilePath
--     OSSTRING_NAME = PosixString   | WindowsString    | OsString
--     WORD_NAME     = PosixChar     | WindowsChar      | OsChar
--     WTOR          = PW            | WW               | OsChar
--     CTOR          = PS            | WS               | OsString



------------------------
-- Separator predicates


#ifdef WINDOWS
-- | Ideal path separator character: '\\'.
#elif defined POSIX
-- | Ideal path separator character: '/'.
#else
-- | Ideal path separator character:
--
--   - on windows: '\\'.
--   - on unix: '/'.
#endif
-- | Ideal path separator character
pathSeparator :: WORD_NAME
pathSeparator = WTOR C.pathSeparator

-- | All path separator characters: @[ '/', '\\']
pathSeparators :: [WORD_NAME]
pathSeparators = WTOR <$> C.pathSeparators

-- | Check if a character is the path separator
--
-- >  (n == '/') == isPathSeparator n
isPathSeparator :: WORD_NAME -> Bool
isPathSeparator (WTOR w) = C.isPathSeparator w

#ifdef WINDOWS
-- | Search path separator: ';'.
#elif defined POSIX
-- | Search path separator: ':'.
#else
-- | Search path separator:
--
--   - on windows: ';'.
--   - on unix: ':'.
#endif
searchPathSeparator :: WORD_NAME
searchPathSeparator = WTOR C.searchPathSeparator

-- | Check if a character is the search path separator
--
-- > (n == ':') == isSearchPathSeparator n
isSearchPathSeparator :: WORD_NAME -> Bool
isSearchPathSeparator (WTOR w) = C.isSearchPathSeparator w


-- | File extension separator '.'.
extSeparator :: WORD_NAME
extSeparator = WTOR C.extSeparator


-- | Check if a character is the file extension separator
--
-- > (n == '.') == isExtSeparator n
isExtSeparator :: WORD_NAME -> Bool
isExtSeparator (WTOR w) = C.isExtSeparator w


------------------------
-- $PATH methods


#ifdef WINDOWS
-- | Take an 'OsString', split it on the 'searchPathSeparator'.
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
#elif defined POSIX
-- | Take an 'OsString', split it on the 'searchPathSeparator'.
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
#else
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
-- | Split a path into (path,file). 'combine' is the inverse
--
-- >>> splitFileName "path\\file.txt"
-- ("path\","file.txt")
-- >>> splitFileName "path\\"
-- ("path\","")
-- >>> splitFileName "file.txt"
-- (".\","file.txt")
--
-- prop> \path -> uncurry combine (splitFileName path) == path || fst (splitFileName path) == ".\\"
#elif defined POSIX
-- | Split a path into (path,file). 'combine' is the inverse
--
-- >>> splitFileName "path/file.txt"
-- ("path/","file.txt")
-- >>> splitFileName "path/"
-- ("path/","")
-- >>> splitFileName "file.txt"
-- ("./","file.txt")
--
-- prop> \path -> uncurry combine (splitFileName path) == path || fst (splitFileName path) == "./"
#else
-- | Split a path into (path,file). 'combine' is the inverse
--
-- > Wiindows: splitFileName "path\\file.txt" == ("path\","file.txt")
-- > Windows: splitFileName "path\\" == ("path\","")
-- > Windows: splitFileName "file.txt" == (".\","file.txt")
-- > Posix: splitFileName "path/file.txt" == ("path/","file.txt")
-- > Posix: splitFileName "path/" == ("path/","")
-- > Posix: splitFileName "file.txt" == ("./","file.txt")
--
-- > uncurry combine (splitFileName path) == path || fst (splitFileName path) == "./"
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
#elif defined POSIX
-- | Drop the file name
--
-- >>> dropFileName "path/file.txt"
-- "path/"
-- >>> dropFileName "file.txt"
-- "./"
#else
-- | Drop the file name
--
-- > Windows: dropFileName "path\\file.txt" == "path\"
-- > Windows: dropFileName "file.txt" == ".\"
-- > Posix: dropFileName "path/file.txt" == "path/"
-- > Posix: dropFileName "file.txt" == "./"
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
#elif defined POSIX
-- | Join two paths together. If the second path is absolute, then returns it, ignoring
--  the first path.
--
-- >>> combine "/" "file"
-- "/file"
-- >>> combine "/path/to" "file"
-- "/path/to/file"
-- >>> combine "file" "/absolute/path"
-- "/absolute/path"
#else
-- | Join two paths together. If the second path is absolute, then returns it, ignoring
--  the first path.
--
-- > Windows: combine "C:\\" "file" == "C:\file"
-- > Windows: combine "C:\\path\\to" "file" == "C:\path\to\file"
-- > Windows: combine "file" "\\absolute\\path" == "\absolute\path"
-- > Posix: combine "/" "file" == "/file"
-- > Posix: combine "/path/to" "file" == "/path/to/file"
-- > Posix: combine "file" "/absolute/path" == "/absolute/path"
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
#elif defined POSIX
-- | Join a split path back together
--
-- prop> \path -> joinPath (splitPath path) == path
--
-- >>> joinPath ["path","to","file.txt"]
-- "path/to/file.txt"
#else
-- | Join a split path back together
--
-- > joinPath (splitPath path) == path
-- > Windows: joinPath ["path","to","file.txt"] == "path\to\file.txt"
-- > Posix: joinPath ["path","to","file.txt"] == "path/to/file.txt"
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
-- ["C:","C:\foo"]
-- >>> takeAllParents "/abs/def/dod"
-- ["\","\abs","\abs\def"]
-- >>> takeAllParents "/foo"
-- ["\"]
-- >>> takeAllParents "/"
-- []
#elif defined POSIX
-- |Get all parents of a path.
--
-- >>> takeAllParents "/abs/def/dod"
-- ["/","/abs","/abs/def"]
-- >>> takeAllParents "/foo"
-- ["/"]
-- >>> takeAllParents "/"
-- []
#else
-- |Get all parents of a path.
--
-- > Windows: takeAllParents "C:\\foo\\bar" == ["C:","C:\foo"]
-- > Windows: takeAllParents "/abs/def/dod" == ["\","\abs","\abs\def"]
-- > Windows: takeAllParents "/foo" == ["\"]
-- > Posix: takeAllParents "/abs/def/dod" == ["/","/abs","/abs/def"]
-- > Posix: takeAllParents "/foo" == ["/"]
-- > takeAllParents "/" == []
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
#elif defined POSIX
-- | Split a path into a drive and a path.
--   \/ is a Drive.
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
#else
-- | Split a path into a drive and a path.
--   On Posix, \/ is a Drive.
--
-- > Windows: splitDrive "/test" == ("","/test")
-- > Windows: splitDrive "C:\\file" == ("C:\","file")
-- > Windows: splitDrive "//test" == ("//test","")
-- > Posix: splitDrive "/test" == ("/","test")
-- > Posix: splitDrive "//test" == ("//","test")
-- > splitDrive "test/file" == ("","test/file")
-- > splitDrive "file" == ("","file")
-- > uncurry (<>) (splitDrive x) == x
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
#elif defined POSIX
-- | Does a path have a drive.
--
-- >>> hasDrive "/foo"
-- True
-- >>> hasDrive "foo"
-- False
--
-- prop> \x -> not (hasDrive x) == BS.null ((\(CTOR x) -> x) $ takeDrive x)
#else
-- | Does a path have a drive.
--
-- > Windows: hasDrive "C:\\foo" == True
-- > Windows: hasDrive "/foo" == False
-- > Posix: hasDrive "/foo" == True
-- > hasDrive "foo" == False
-- > not (hasDrive x) == null (takeDrive x)
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
#elif defined POSIX
-- | Is an element a drive
--
-- >>> isDrive "/"
-- True
-- >>> isDrive "/foo"
-- False
#else
-- | Is an element a drive
--
-- > Windows: isDrive "C:" == True
-- > Windows: isDrive "/" == False
-- > Posix: isDrive "/" == True
-- > isDrive "/foo" == False
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
#elif defined POSIX
-- | Add a trailing path separator.
--
-- >>> addTrailingPathSeparator "/path"
-- "/path/"
-- >>> addTrailingPathSeparator "/path/"
-- "/path/"
-- >>> addTrailingPathSeparator "/"
-- "/"
#else
-- | Add a trailing path separator.
--
-- > Windows: addTrailingPathSeparator "/path" == "/path\"
-- > Posix: addTrailingPathSeparator "/path" == "/path/"
-- > addTrailingPathSeparator "/path/" == "/path/"
-- > addTrailingPathSeparator "/" == "/"
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
#elif defined POSIX
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
#else
-- | Remove trailing path separators
--
-- > dropTrailingPathSeparator "/path/" == "/path"
-- > dropTrailingPathSeparator "/path////" == "/path"
-- > dropTrailingPathSeparator "/" == "/"
-- > Posix: dropTrailingPathSeparator "//" == "/"
-- > Windows: dropTrailingPathSeparator "//" == "//"
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
-- "\test\file\..\bob\fred\"
-- >>> normalise "../bob/fred/"
-- "..\bob\fred\"
-- >>> normalise "./bob/fred/"
-- "bob\fred\"
-- >>> normalise "./bob////.fred/./...///./..///#."
-- "bob\.fred\...\..\#."
-- >>> normalise "."
-- "."
-- >>> normalise "./"
-- ".\"
-- >>> normalise "./."
-- ".\"
-- >>> normalise "/./"
-- "\"
-- >>> normalise "/"
-- "\"
-- >>> normalise "bob/fred/."
-- "bob\fred\"
-- >>> normalise "//home"
-- "\\home"
#elif defined POSIX
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
#else
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
#elif defined POSIX
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
#else
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
#elif defined POSIX
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
#else
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
#elif defined POSIX
-- | Check if a path is absolute
--
-- >>> isAbsolute "/path"
-- True
-- >>> isAbsolute "path"
-- False
-- >>> isAbsolute ""
-- False
#else
-- | Check if a path is absolute
--
-- > Windows: isAbsolute "C:\\path" == True
-- > Windows: isAbsolute "/path" == False
-- > Posix: isAbsolute "/path" == True
-- > isAbsolute "path" == False
-- > isAbsolute "" == False
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


#ifdef POSIX
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
isSpecialDirectoryEntry (CTOR path) = C.isSpecialDirectoryEntry path
#endif

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


#ifdef POSIX
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
hiddenFile (CTOR fp) = C.hiddenFile fp
#endif
