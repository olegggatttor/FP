{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Rank2Types #-}

module FileSystemLenses where

import Lens.Micro (Lens', Traversal', (^.), (^..), (^?!), lens, filtered, each)
import Data.List (intercalate)
import System.Directory (doesFileExist, doesDirectoryExist, listDirectory)
import System.FilePath.Posix (takeFileName, (</>))

data FS
  = Dir
  { _name     :: FilePath  -- название папки, не полный путь
  , _contents :: [FS]
  }
  | File
  { _name     :: FilePath  -- название файла, не полный путь
  }


name
  :: Lens' FS FilePath
name = lens _name (\fs newName -> fs { _name = newName })

contents
  :: Traversal' FS [FS]
contents f dir@Dir{} = (\newContents -> dir { _contents = newContents }) <$> f (_contents dir)
contents _ fs        = pure fs

fileFocus
  :: Traversal' FS FS
fileFocus = \f file -> case file of
                         (File _) -> f file
                         _        -> pure file

dirFocus
  :: Traversal' FS FS
dirFocus = \f file -> case file of
                        (Dir _ _) -> f file
                        _        -> pure file

instance Show FS where
  show = showRecur ""
    where
      showRecur
        :: String
        -> FS
        -> String
      showRecur prefix (File n) = prefix ++ "|" ++ n
      showRecur prefix (Dir n files) = prefix ++ "|" ++ n ++ "\n" ++
                                       intercalate "\n" (map (showRecur (prefix ++ " ")) files)

scanFiles
  :: FilePath
  -> IO FS
scanFiles path = do
  !isDir  <- doesDirectoryExist path
  if isDir
    then do
      files <- listDirectory path
      if null files
      then return $ Dir (takeFileName path) []
      else do
        readyFiles <- mapM (scanFiles . (path </>)) files
        return $ Dir (takeFileName path) readyFiles
  else return $ File (takeFileName path)

scanFS
  :: FilePath
  -> IO FS
scanFS path = do
  !isFile <- doesFileExist path
  !isDir  <- doesDirectoryExist path
  if isFile
  then error "No such directory. Only file was found."
  else if isDir
       then scanFiles path
       else error "No such directory."

filex = File "a"

direx = Dir "b" [filex, filex]

dire = Dir "c" [direx, filex, filex]

cd
  :: FilePath
  -> Traversal' FS FS
cd dir = contents . each . dirFocus . filtered ((== dir) . (^. name))

ls
  :: Traversal' FS FilePath
ls = contents . each . name

file
  :: FilePath
  -> Traversal' FS FilePath
file fileName = contents . each . fileFocus . name . filtered (== fileName)
