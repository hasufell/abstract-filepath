module Main (main) where


import           Test.Tasty.Bench

import qualified Short.BenchAll as SBA
import qualified Short.Word16.BenchAll as SWBA



main :: IO ()
main = do
  defaultMain
    (SBA.benchmarks ++ SWBA.benchmarks)
