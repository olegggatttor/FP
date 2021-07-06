{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Commands
  ( Commands(..)
  ) where


import Control.Exception()
import Control.Monad ( filterM )
import Data.List ( intercalate, sort )
import System.Directory ( Permissions(..) )
import System.FilePath.Posix ( pathSeparator )
import Data.Time.Clock ( UTCTime(..) )

-- | infixl $
infixl 0 $$
($$)
  :: (a -> b) -- ^ Takes function
  -> a        -- ^ Takes argument
  -> b        -- ^ Applies function to argument
f $$ x = f x

-- | Converts Permissions to String representation
getPermissions'
  :: Permissions  -- ^ Given Permissions
  -> String       -- ^ Resulting String
getPermissions' p = concat [ convert (readable p) "r", convert (writable p) "w"
                           , convert (executable p) "e", convert (searchable p) "s"]
  where
    convert
      :: Bool
      -> String
      -> String
    convert True  str = str
    convert False _   = "-"

-- | FileSystem Commands type class.
class Monad m => Commands m where
  -- | Returns current directory in given m
  getCurDir
    :: m FilePath -- ^ Current directory

  -- | Extend given path to absolute
  extendPath
    :: FilePath    -- ^ Given path
    -> m FilePath  -- ^ Resulting path

  -- | Changes current directory
  setCurDir
    :: FilePath   -- ^ Sub directory or .. (Relative path)
    -> m ()       -- ^ Returns ()

  -- | Printing info
  logMsg
    :: String  -- ^ Info to print
    -> m ()    -- ^ Returns ()

  -- | Returns all Files in given directory
  getDirFiles
    :: FilePath      -- ^ Given dir
    -> m [FilePath]  -- ^ Resulting list of files

  -- | Creating dir at given path
  createDir
    :: FilePath  -- ^ Given path
    -> m ()      -- ^ Returns ()

  -- | Checks if file exists
  checkDoesFileExist
    :: FilePath  -- ^ File path
    -> m Bool    -- ^ Returns Bool - does file exists or not

  -- | Checks if dir exists
  checkDoesDirExist
    :: FilePath -- ^ Dir path
    -> m Bool   -- ^ Returns Bool - does dir exists or not

  -- | Gets user input
  getInput
    :: m String  -- ^ Returns given input

  -- | Reads from file at given path
  readFromFile
    :: FilePath  -- ^ Given file path
    -> m String  -- ^ Resulting content of file

  -- | Writes content to given file
  writeToFile
    :: FilePath  -- ^ Given file's path
    -> String    -- ^ Content to write
    -> m ()      -- Returns ()

  -- | Removes file at given path
  rmFile
    :: FilePath  -- ^ Given file's path
    -> m ()      -- ^ Returns ()

  -- | Removes dir at given path
  rmDir
    :: FilePath  -- ^  Dir's path
    -> m ()      -- ^ Returns ()

  -- | Takes list of subdirectories in current directory
  -- | and finds all pathes to file with given name
  findAllFiles
    :: [FilePath]    -- ^ Given list of subdirs
    -> FilePath      -- ^ Given file name
    -> m [FilePath]  -- ^ Returns list of pathes to the file with given file name

  -- | Receives canonicalized path
  receiveCanonicalizedPath
    :: FilePath -- ^ Usual path
    -> m String -- ^ Return canonicalized path

  -- | Receives permissions of file at given path
  receivePermissions
    :: FilePath       -- ^ Given path
    -> m Permissions  -- ^ Returns Permissions of given file

  -- | Receives file size
  receiveFileSize
    :: FilePath  -- ^ Given file
    -> m Integer -- ^ Resulting file size

  -- | Receives file type
  receiveFileType
    :: FilePath  -- ^ Given file
    -> m String  -- ^ Returns file type

  -- | Receives last modification time
  receiveModTime
    :: FilePath  -- ^ Given file
    -> m UTCTime -- ^ Last modification time

  -- | Performs file ot dir command
  fileOrDirCommand
    :: FilePath -- ^ Fiven path
    -> m a      -- ^ Dir action (if file at given path is dir)
    -> m a      -- ^ File action (if file at given path is not dir)
    -> m a      -- ^ Exception action (if file at given path is not dir and not file)
    -> m a      -- ^ Returns a
  fileOrDirCommand path dirAction fileAction onException = do
    isFile <- checkDoesFileExist path
    isDir  <- checkDoesDirExist path
    case isDir of
      True  -> dirAction
      False -> case isFile of
                 True  -> fileAction
                 False -> onException

  -- | Changes current directory
  cd
    :: FilePath -- ^ New path
    -> m ()     -- ^ Returns ()
  cd ".." = setCurDir ".."
  cd dirPath = do
    fullPath <- extendPath dirPath
    fileOrDirCommand $$ fullPath
                     $$ setCurDir fullPath
                     $$ logMsg "[ERROR]: No such directory. Only file found."
                     $$ logMsg "[ERROR]: No such directory."

  -- | Lists directory at given path
  ls
    :: FilePath -- ^ Given path
    -> m ()     -- ^ Returns ()
  ls dirPath = do
    fullPath <- extendPath dirPath
    fileOrDirCommand $$ fullPath
                     $$ (getDirFiles fullPath >>= \x -> logMsg $ intercalate "\r\n" x)
                     $$ logMsg "[ERROR]: No such directory. Only file found."
                     $$ logMsg "[ERROR]: No such directory."

  -- | Lists current directory
  dir
    :: m ()
  dir = ls "."

  -- | Creates directory at given path
  mkdir
    :: FilePath -- ^ given path
    -> m ()     -- ^ Returns ()
  mkdir dirPath = do
    fullPath <- extendPath dirPath
    fileOrDirCommand $$ fullPath
                     $$ logMsg "[INFO]: Such directory already exists."
                     $$ logMsg "[ERROR]: Ok. File with the same name already exists."
                     $$ logMsg "[INFO]: Ok. Creating directory..." >> createDir fullPath

  -- | Creates file at given path
  touch
    :: FilePath -- ^ Given path
    -> m ()     -- ^ Returns ()
  touch filePath = makeFile
    where
      makeFile
        :: Commands m
        => m ()
      makeFile = do
        fullPath <- extendPath filePath
        fileOrDirCommand $$ fullPath
                         $$ logMsg "[ERROR]: Directory with the same name already exists."
                         $$ logMsg "[INFO]: File with the same name already exists.\r\n\
                                   \[INFO]: Do you really want to overwrite it? (y/n)"
                         >> askToOverwrite
                         $$ logMsg "[INFO]: Ok. Creating..." >> writeToFile fullPath ""

      askToOverwrite
        :: Commands m
        => m ()
      askToOverwrite = do
        fullPath <- extendPath filePath
        ans      <- getInput
        case ans of
          "y" -> logMsg "[INFO]: Ok. Creating..." >> writeToFile fullPath ""
          "n" -> logMsg "[INFO]: Ok. Canceling..."
          _   -> logMsg "[INFO]: Only (y/n) answers are accepted." >> askToOverwrite

  cat
    :: FilePath
    -> m ()
  cat filePath = do
    fullPath <- extendPath filePath
    fileOrDirCommand $$ fullPath
                     $$ logMsg "[ERROR]: No such file. Only dir was found."
                     $$ readFromFile fullPath >>= logMsg
                     $$ logMsg "[ERROR]: No such file."

  rm
    :: FilePath
    -> m ()
  rm fileOrDirPath = makeRm
    where
      makeRm
        :: Commands m
        => m ()
      makeRm = do
        fullPath <- extendPath fileOrDirPath
        fileOrDirCommand $$ fullPath
                         $$ logMsg "[INFO]: Ok. Removing directory..." >> rmDir fullPath
                         $$ logMsg "[INFO]: Ok. Removing file..." >> rmFile fullPath
                         $$ logMsg "[ERROR]: No such file or directory."

  write
    :: FilePath
    -> String
    -> m ()
  write filePath text = writeTextToFile
    where
      writeTextToFile
        :: Commands m
        => m ()
      writeTextToFile = do
        fullPath <- extendPath filePath
        fileOrDirCommand $$ fullPath
                         $$ logMsg "[ERROR]: Impossible to write into directory."
                         $$ logMsg "[INFO]: Ok. Writing..."
                         >> writeToFile fullPath text
                         $$ logMsg "[ERROR]: Such file does not exists."

  -- | Finds all pathes to file with given name
  find
    :: FilePath  -- ^ Given name
    -> m ()      -- ^ Returns ()
  find fileName = tryToFind
    where
      tryToFind
        :: Commands m
        => m ()
      tryToFind = do
        full <- extendPath "."
        allDirs   <- getAllDirs full
        allPathes <- findAllFiles (full : allDirs) fileName >>= return . sort
        case (null allPathes) of
          True  -> logMsg "[INFO]: Can not find files with such name in this directory."
          False -> logMsg $ intercalate "\r\n" allPathes

      getAllDirs
        :: Commands m
        => FilePath
        -> m [FilePath]
      getAllDirs path = do
        curDirFiles <- getDirFiles path
        dirs        <- filterM checkDoesDirExist $ fmap ((path ++) . (pathSeparator :)) curDirFiles
        subDirs <- traverse (\x -> getAllDirs x) dirs
        return $ dirs ++ concat subDirs

  -- | Returns file or dir info at given path
  info
    :: FilePath  -- ^ Given path
    -> m ()      -- ^ Returns ()
  info path = extendPath path >>= \x -> fileOrDirCommand x dirAction fileAction onExcept
    where
      dirAction
        :: Commands m
        => m ()
      dirAction = do
        fullPath    <- extendPath path
        canonPath   <- receiveCanonicalizedPath fullPath
        permissions <- receivePermissions fullPath
        size        <- receiveFileSize fullPath
        amount      <- fmap length $ getDirFiles fullPath
        logMsg      $ "---Directory Info---" ++
                      "\r\nPath: " ++ canonPath ++
                      "\r\nPermissions: " ++ (getPermissions' permissions)
                      ++ "\r\nSize: " ++ (show size) ++ " bytes" ++
                      "\r\nAmount of files: " ++ (show amount) ++
                      "\r\n--------------------"

      fileAction
        :: Commands m
        => m ()
      fileAction = do
        fullPath    <- extendPath path
        canonPath   <- receiveCanonicalizedPath fullPath
        permissions <- receivePermissions fullPath
        size        <- receiveFileSize fullPath
        fileType    <- receiveFileType fullPath
        modTime     <- receiveModTime fullPath
        logMsg      $ "------File Info------" ++
                      "\r\nPath: " ++ canonPath ++
                      "\r\nPermissions: " ++ (getPermissions' permissions)
                      ++ "\r\nSize: " ++ (show size) ++ " bytes" ++
                      "\r\nType: " ++ (show fileType) ++
                      "\r\nLast mod time: " ++ (show modTime) ++
                      "\r\n---------------------"

      onExcept = logMsg "[ERROR]: No such file or directory."
