{-# LANGUAGE CPP #-}

import Test.DocTest

main = doctest [ "-ilib"
               , "-XTypeSynonymInstances"
               , "-XOverloadedStrings"
               , "lib/AFP/AbstractFilePath/Internal/Windows.hs"
               , "lib/AFP/AbstractFilePath/Internal/Posix.hs"
               , "lib/AFP/AbstractFilePath/Posix.hs"
               , "lib/AFP/AbstractFilePath/Windows.hs"
               ]
