module Main where

import Test.Hspec (hspec)
import FileSystemSpec

main :: IO ()
main = hspec $ specFileSystem
