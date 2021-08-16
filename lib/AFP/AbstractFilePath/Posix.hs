{-# LANGUAGE CPP #-}
#define MODULE_NAME     Posix
#define FILEPATH_NAME   PosixFilePath
#define OSSTRING_NAME   PosixString
#define WORD_NAME       PosixWord
#define CTOR            PS
#define WTOR            PW
#define IS_WINDOWS      False
#undef WINDOWS
#include "Common.hs"
#undef MODULE_NAME
#undef FILEPATH_NAME
#undef OSSTRING_NAME
#undef CTOR
#undef IS_WINDOWS

