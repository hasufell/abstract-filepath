module Main (main) where


import           GHC.IO.Encoding
import           Test.Tasty.Bench

import qualified Short.BenchAll as SBA
import qualified Short.Word16.BenchAll as SWBA



main :: IO ()
main = do
  setLocaleEncoding utf8
  defaultMain
    (SBA.benchmarks ++ SWBA.benchmarks)
