module TestResults where

import FileSystem
import System.Directory ( Permissions(..), emptyPermissions )
import ToyFS

addFiles
  :: FileSystem
  -> [FileSystem]
  -> FileSystem
addFiles (File _) _ = error "File has no children."
addFiles (Dir params) files = Dir $ params{ children = files ++ (children params)
                                          , cntFiles = (cntFiles params) + length files
                                          , dirSize  = sum $ fmap getSize files
                                          }
  where
    getSize
      :: FileSystem
      -> Integer
    getSize (File params) = fileSize params
    getSize (Dir params)  = dirSize params

dirA
  :: FileSystem
dirA = emptyDir "a"

dirB
  :: FileSystem
dirB = emptyDir "b"

dirC
  :: FileSystem
dirC = emptyDir "c"

file1
  :: FileSystem
file1 = emptyFile "test.txt"

file2
  :: FileSystem
file2 = emptyFile "haskell.hs"

textedFile
  :: FileSystem
textedFile = case emptyFile "texted.txt" of
               (File params) -> File $ params{ fileContent = "Hello world!", fileSize = 12 }
               otherwise     -> error "File expected."

rootDir
  :: FileSystem
rootDir = emptyDir "~"

mkdirTest1
  :: FileSystem
mkdirTest1 = addFiles rootDir [dirA, dirB, dirC]

mkdirTest23
  :: FileSystem
mkdirTest23 = addFiles rootDir [addFiles dirA [dirB], addFiles dirB [dirA, dirB]]

touchTest1
  :: FileSystem
touchTest1 = addFiles rootDir [addFiles dirA [file1], file2]

touchTest2
  :: FileSystem
touchTest2 = addFiles rootDir [file1]

rmTest1
  :: FileSystem
rmTest1 = addFiles rootDir [dirA]

rmTest2
  :: FileSystem
rmTest2 = addFiles rootDir [addFiles dirA [dirB]]

writeCatTest1
  :: FileSystem
writeCatTest1 = addFiles rootDir [textedFile]

findTest1
  :: FileSystem
findTest1 = addFiles rootDir [addFiles dirA
                               [addFiles dirB
                                 [addFiles dirC [file1]]
                               ]
                             , addFiles dirB
                               [addFiles dirA [file1], dirC]
                             , addFiles dirC [file1]
                             , file1
                             ]

findTest2
  :: FileSystem
findTest2 = addFiles rootDir [addFiles dirA
                              [addFiles dirB [dirC]]
                             , addFiles dirB [dirA, dirC]
                             , dirC
                             ]
