{-# LANGUAGE CPP #-}

import Test.DocTest

main = doctest [ "-ilib"
               , "-XTypeSynonymInstances"
               , "-XOverloadedStrings"
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
               , "lib/AFP/AbstractFilePath/Internal/Windows.hs"
#else
               , "lib/AFP/AbstractFilePath/Internal/Posix.hs"
#endif
               --, "lib/AFP/AbstractFilePath/Posix.hs"
               , "lib/AFP/AbstractFilePath/Windows.hs"
               ]
