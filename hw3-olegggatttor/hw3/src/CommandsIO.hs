{-# LANGUAGE FlexibleInstances #-}

module CommandsIO where

import Commands
import System.Directory ( listDirectory, createDirectoryIfMissing
                        , doesFileExist, doesDirectoryExist
                        , removeDirectoryRecursive, removeFile, canonicalizePath
                        , getFileSize, getModificationTime, getPermissions, findFiles
                        , canonicalizePath
                        )
import Control.Exception()
import System.FilePath.Posix ( takeExtension, (</>) )
import Data.List()
import Data.IORef( IORef, readIORef, modifyIORef )
import Control.Monad.Trans.Reader( ReaderT(..), ask )
import Control.Monad.Trans ( liftIO )

instance Commands (ReaderT (IORef FilePath) IO) where
  getCurDir = ask >>= liftIO . readIORef >>= liftIO . canonicalizePath

  extendPath path = do
    pathVar <- ask
    curDir <- liftIO $ readIORef pathVar
    let fullPath = curDir </> path
    return fullPath

  setCurDir path = do
    pathVar <- ask
    liftIO $ modifyIORef pathVar (\_ -> path)

  logMsg = liftIO . putStrLn

  getDirFiles = liftIO . listDirectory

  createDir = liftIO . createDirectoryIfMissing True

  checkDoesFileExist = liftIO . doesFileExist

  checkDoesDirExist = liftIO . doesDirectoryExist

  getInput = liftIO getLine

  writeToFile path = liftIO . writeFile path

  readFromFile = liftIO . readFile

  rmFile = liftIO . removeFile

  rmDir = liftIO . removeDirectoryRecursive

  findAllFiles pathes = liftIO . findFiles pathes

  receiveCanonicalizedPath = liftIO . canonicalizePath

  receivePermissions = liftIO . getPermissions

  receiveFileSize = liftIO . getFileSize

  receiveFileType = liftIO . return . takeExtension

  receiveModTime = liftIO . getModificationTime
