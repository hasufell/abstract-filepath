{-# LANGUAGE CPP #-}
#define MODULE_NAME     Posix
#define PLATFORM_STRING PosixString
#define PLATFORM_WORD   PosixChar
#define IS_WINDOWS      False
#include "Common.hs"
#undef MODULE_NAME
#undef FILEPATH_NAME
#undef OSSTRING_NAME
#undef IS_WINDOWS
#undef WINDOWS
