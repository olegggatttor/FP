module Main
  ( main
  ) where

import CommandLineInterface
import FileSystem()
import Commands
import System.IO.Error ( isDoesNotExistError, isPermissionError, isFullError, isAlreadyExistsError)
import System.Environment ( getArgs )
import System.Directory ( canonicalizePath )
import CommandsIO()
import Data.IORef ( newIORef )
import Control.Monad.Trans.Reader ( ReaderT(..) )
import FSException
import ToyFS

-- | Entry point.
main :: IO ()
main = do
  fsType <- getArgs
  case fsType of
    ("toy":_)          -> (runReaderT  (interactor toyCatcher)) =<< newIORef ("~", toyFS)
    ("real":absPath:_) -> (runReaderT  (interactor ioCatcher)) =<< newIORef =<< canonicalizePath absPath
    _                  -> putStrLn "Invalid arguments.\r\nUsage: [toy| real INITPATH]."

-- | Exception handler for Real filesystem.
ioCatcher
  :: (Commands m)  -- ^ m must be instance of Command
  => IOError       -- ^ Catches IOError
  -> m ()          -- ^ Returns m ()
ioCatcher ex | isDoesNotExistError  ex
                  = logMsg "[ERROR]: There is no path referring to the working directory."
             | isPermissionError    ex
                  = logMsg "[ERROR]: The process has insufficient privileges to perform the operation."
             | isFullError          ex
                  = logMsg "[ERROR]: Insufficient resources are available to perform the operation."
             | isAlreadyExistsError ex
                  = logMsg "[ERROR]: File or directory with the same name already exists."
             | otherwise
                  = logMsg "[ERROR]: I/O error occured."

-- | Exception handler for toy filesystem.
toyCatcher
  :: (Commands m) -- ^ m must be instance of Command
  => FSException  -- ^ Catches FSException
  -> m ()         -- ^ Returns m ()
toyCatcher DirDoesNotExistEx  = logMsg "[ERROR]: There is no path referring to the working directory."
toyCatcher FileDoesNotExistEx = logMsg "[ERROR]: There is no path referring to the file."
toyCatcher NoSuchFileOrDirEx  = logMsg "[ERROR]: There is no path referring to the file or firectory."
toyCatcher EndOfFileSystem    = logMsg "[ERROR]: \"~\" is the top of file system. Impossible to go upper."
toyCatcher InvalidName        = logMsg "[ERROR]: Invalid name for file or directory."
