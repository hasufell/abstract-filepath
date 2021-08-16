{-# LANGUAGE CPP #-}
#define MODULE_NAME     Windows
#define FILEPATH_NAME   WindowsFilePath
#define OSSTRING_NAME   WindowsString
#define WORD_NAME       WindowsWord
#define CTOR            WS
#define WTOR            WW
#define IS_WINDOWS      True
#define WINDOWS
#include "Common.hs"
#undef MODULE_NAME
#undef FILEPATH_NAME
#undef OSSTRING_NAME
#undef CTOR
#undef IS_WINDOWS
#undef WINDOWS
