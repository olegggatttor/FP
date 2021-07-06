{-# LANGUAGE  FlexibleInstances #-}

module FileSystem
  ( FileSystem(..)
  , FileParams(..)
  , DirParams(..)
  , emptyDir
  , emptyFile
  ) where

import Data.Time ( UTCTime(..) )
import Data.Time.Calendar ( fromGregorian )
import Data.Time.Clock ( secondsToDiffTime )
import System.Directory ( Permissions(..), emptyPermissions )
import Control.Exception()
import Control.Monad.Trans.Reader ( ReaderT(..), ask)
import Control.Monad.Trans ( liftIO )
import Control.Monad ( when )
import Data.Time.Clock ( getCurrentTime )
import Data.IORef (readIORef, modifyIORef, IORef )
import qualified Data.List as Ls ( intercalate, find, sort )
import System.FilePath.Posix ( pathSeparator, takeExtension, (</>) )
import Data.List.Split ( splitOn )
import Commands
import Data.Maybe()
import Control.Applicative()
import Data.Functor ( ($>) )
import FSException
import Control.Monad.Catch ( throwM )

type ModificationTime =  UTCTime
type FileType =  String
type FileSize = Integer
type AmountOfFiles = Int

-- | File parameters data
data FileParams
  = FileParams
  { filePath        :: FilePath
  , filePermissions :: Permissions
  , fileType        :: FileType
  , modifyTime      :: ModificationTime
  , fileSize        :: FileSize
  , fileContent     :: String
  }
  deriving Show

-- Does not compare ModificationTime due to testing problems.
instance Eq FileParams where
  (FileParams path1 perms1 type1 _ sz1 text1) == (FileParams path2 perms2 type2 _ sz2 text2) =
    path1  == path2  &&
    perms1 == perms2 &&
    type1  == type2  &&
    sz1    == sz2    &&
    text1  == text2

-- | Dir parameters data
data DirParams
  = DirParams
  { dirPath        :: FilePath
  , dirPermissions :: Permissions
  , dirSize        :: FileSize
  , cntFiles       :: AmountOfFiles
  , children       :: [FileSystem]
  }
  deriving Show

instance Eq DirParams where
  (DirParams path1 perms1 sz1 cnt1 ch1) == (DirParams path2 perms2 sz2 cnt2 ch2) =
    path1         == path2  &&
    perms1        == perms2 &&
    sz1           == sz2    &&
    cnt1          == cnt2   &&
    (Ls.sort ch1) == (Ls.sort ch2)

-- | FileSystem data
data FileSystem
  = File FileParams
  | Dir DirParams
  deriving (Show, Eq)

instance Ord FileSystem where
  (File p1) <= (File p2) = (filePath p1) <= (filePath p2)
  (Dir p1)  <= (File p2) = (dirPath p1)  <= (filePath p2)
  (File p1) <= (Dir p2)  = (filePath p1) <= (dirPath p2)
  (Dir p1)  <= (Dir p2)  = (dirPath p1)  <= (dirPath p2)


-- | Creates empty dir at given path
emptyDir
  :: FilePath    -- ^ Given path
  -> FileSystem  -- ^ Returns empty dir
emptyDir path = Dir DirParams { dirPath = path
                              , dirPermissions = emptyPermissions{ readable = True, writable = True }
                              , dirSize = 0
                              , cntFiles = 0
                              , children = []
                              }

-- | Mock UTCTime for testing.
mockTime
  :: UTCTime -- ^ Returns mock time
mockTime = UTCTime (fromGregorian 2020 12 31) (secondsToDiffTime 0)

-- | Creates empty file at given path
emptyFile
  :: FilePath    -- ^ File path
  -> FileSystem  -- ^ Returns empty file
emptyFile path = File FileParams { filePath = path
                                 , filePermissions = emptyPermissions{ readable = True, writable = True }
                                 , fileType = takeExtension path
                                 , modifyTime = mockTime
                                 , fileSize = 0
                                 , fileContent = ""
                                 }

-- | Checks if given dir has given name
isDirWithName
  :: FilePath   -- ^ Given name
  -> FileSystem -- ^ Given dir
  -> Bool       -- ^ Result Bool
isDirWithName name (Dir params) = (name) == (dirPath params)
isDirWithName _    (File _)     = False

-- | Checks if given file has given name
isFileWithName
  :: FilePath   -- ^ Given name
  -> FileSystem -- ^ Given dir
  -> Bool       -- ^ Result Bool
isFileWithName name (File params) = (name) == (filePath params)
isFileWithName _    (Dir _)     = False

-- | Tries to find file or dir at given path
findObject
  :: [FilePath]        -- ^ Splitted path
  -> FileSystem        -- ^ Current directory
  -> Maybe FileSystem  -- ^ Result
findObject []         x = Just x
findObject (cur:[])   (Dir fs) = Ls.find (\obj -> isFileWithName cur obj ||
                                                  isDirWithName cur obj) (children fs)
findObject (cur:rest) (Dir fs) = do
  case (Ls.find (isDirWithName cur) (children fs)) of
    Just foundDir -> findObject rest foundDir
    Nothing  -> Nothing
findObject _ x = Just x

-- | Replaces dir with given one
replaceFolder
  :: FileSystem -- ^ Current directory
  -> FileSystem -- ^ Child of current to remove
  -> FileSystem -- ^ Child to add to current
  -> FileSystem -- ^ Resturns changed directory
replaceFolder (Dir params) (Dir toRmParams) toAdd@(Dir addParams) = Dir $
                                                          params{ children = newChildren
                                                                , dirSize = ( (dirSize params)
                                                                            - (dirSize toRmParams)
                                                                            + (dirSize addParams))
                                                                , cntFiles = length newChildren
                                                                }
  where
    newChildren
      :: [FileSystem]
    newChildren = (:) toAdd (filter (not . isDirWithName (dirPath toRmParams)) (children params))
replaceFolder x _ _ = x


-- | Changes FS structure at given path
changeStructure
  :: [FilePath]                             -- ^ Given path
  -> FileSystem                             -- ^ Current directory
  -> (FileSystem -> FilePath -> FileSystem) -- ^ Function for changing
  -> FileSystem                             -- ^ Resulting modified FS
changeStructure _          x@(File _)      _ = x
changeStructure []         x               f = f x ""
changeStructure (cur:[])   x               f = f x cur
changeStructure (cur:rest) folder@(Dir fs) f = do
  case (Ls.find (isDirWithName cur) (children fs)) of
    Just foundDir -> replaceFolder folder foundDir (changeStructure rest foundDir f)
    Nothing  -> replaceFolder folder (emptyDir "") (changeStructure rest (emptyDir cur) f)

-- | Deletes file or dir in given directory
deleteFileOrDir
  :: FileSystem -- ^ Given directory
  -> FilePath   -- ^ Name for delition
  -> FileSystem -- ^ Resulting FS
deleteFileOrDir (Dir params) name = Dir $ params { children = newChildren
                                            , dirSize = (dirSize params) - getFileSize
                                            , cntFiles = length newChildren
                                            }
  where
    newChildren
      :: [FileSystem]
    newChildren = filter (\obj -> not (isFileWithName name obj || isDirWithName name obj)) (children params)

    getFileSize
      :: Integer
    getFileSize = case Ls.find (\obj -> isFileWithName name obj || isDirWithName name obj)
                               (children params) of
      Just (File fileParams) -> fileSize fileParams
      Just (Dir dirParams) -> dirSize dirParams
      Nothing                -> 0
deleteFileOrDir x _ = x

-- | Replaces file at given path
replaceFile
  :: FileSystem -- ^ Working dir
  -> FilePath   -- ^ File name
  -> String     -- ^ File content
  -> UTCTime    -- ^ Modify time
  -> FileSystem -- ^ Changed directory with new file
replaceFile (Dir params) name content time = Dir $ params { children = newChildren
                                                     , dirSize = ( dirSize params)
                                                                 - getFileSize
                                                                 + (toInteger $ length content)
                                                     , cntFiles = length newChildren}
  where
    newChildren
      :: [FileSystem]
    newChildren = (:) mkfile (filter (not . isFileWithName name) (children params))

    mkfile
      :: FileSystem
    mkfile = File $ FileParams { filePath = name
                               , filePermissions = emptyPermissions{ readable = True, writable = True }
                               , fileType = takeExtension name
                               , modifyTime = time
                               , fileSize = toInteger $ length content
                               , fileContent = content
                               }

    getFileSize
      :: Integer
    getFileSize = case (Ls.find (isFileWithName name) (children params)) of
                    Just (File x) -> fileSize x
                    _     -> 0
replaceFile x _ _ _ = x

-- | Normalizes given path
normalize
  :: FilePath -- ^ Given path
  -> [String] -- ^ Splitted normalized path
normalize path = filter ( /= ".") $ splitOn [pathSeparator] path

-- | Returns FS name (file or dir)
getNames
  :: FileSystem -- ^ Path
  -> FilePath   -- ^ Resulting name
getNames (File params) = filePath params
getNames (Dir params)  = dirPath params

-- | Joins path
join
  :: [FilePath] -- ^ Splitted path
  -> FilePath   -- ^ Resulting path
join = Ls.intercalate [pathSeparator]

-- | Safe init function
safeInit
  :: [a] -- ^ Takes list
  -> [a] -- ^ Return init or [] if list was empty
safeInit [] = []
safeInit x  = init x

-- | Monadic field reciever
receiveDirOrFileField
  :: (FileParams -> a)                           -- ^ File getter
  -> (DirParams -> a)                            -- ^ Dir getter
  -> FilePath                                    -- ^ Path
  -> ReaderT (IORef (FilePath, FileSystem)) IO a -- ^ Resulting field
receiveDirOrFileField fileGet dirGet path = do
  fsContext <- ask
  (_, fs) <- liftIO $ readIORef fsContext
  let normalized = normalize path
  case findObject (tail $ normalized) fs of
    Just (Dir dirParams)   -> return $ dirGet dirParams
    Just (File fileParams) -> return $ fileGet fileParams
    _                      -> throwM NoSuchFileOrDirEx

-- | Checks if object at given path exists
doesObjectExist
  :: ([FilePath] -> FileSystem -> Maybe FileSystem)  -- ^ Extractor of dir or file
  -> (Maybe FileSystem -> Bool)                      -- ^ Checker
  -> FilePath                                        -- ^ Path
  -> ReaderT (IORef (FilePath, FileSystem)) IO Bool  -- ^ Returns Bool
doesObjectExist f check path = do
  fsContext <- ask
  (_, fs) <- liftIO $ readIORef fsContext
  let normalized = normalize path
  return . check $ f (tail $ normalized) fs

-- | Performs file action or throws error
fileActionOrError
  :: FilePath                                                     -- ^ Path
  -> (FileParams -> ReaderT (IORef (FilePath, FileSystem)) IO a)  -- ^ Action
  -> ReaderT (IORef (FilePath, FileSystem)) IO a                  -- ^ Action on error
  -> ReaderT (IORef (FilePath, FileSystem)) IO a                  -- ^ Result
fileActionOrError path f exOrMkFile = do
  fsContext <- ask
  (_, fs) <- liftIO $ readIORef fsContext
  let normalized = normalize path
  case findObject (tail $ normalized) fs of
    Just (File params) -> f params
    _          -> exOrMkFile

-- | Validates given name
validateName
  :: [String]                                       -- ^ Splitted name
  -> (ReaderT (IORef (FilePath, FileSystem)) IO ()) -- ^ Throws error or returns ()
validateName path = do
  when ("~" == last path) (throwM InvalidName)
  when (".." == last path) (throwM InvalidName)
  when ("." == last path) (throwM InvalidName)
  when ("" == last path) (throwM InvalidName)

instance Commands (ReaderT (IORef (FilePath, FileSystem)) IO) where
  getCurDir = ask >>= (liftIO . readIORef) >>= return . fst

  extendPath path = do
    fsContext <- ask
    (cur, _) <- liftIO $ readIORef fsContext
    let newPath = cur </> path
    return newPath

  logMsg text = liftIO $ putStrLn text

  getInput    = return "n"

  checkDoesFileExist = doesObjectExist findObject isFile
    where
      isFile
        :: Maybe FileSystem
        -> Bool
      isFile (Just (File _)) = True
      isFile _        = False

  checkDoesDirExist = doesObjectExist findObject isDir
    where
      isDir
        :: Maybe FileSystem
        -> Bool
      isDir (Just (Dir _)) = True
      isDir _        = False

  readFromFile path = fileActionOrError path (return . fileContent) (throwM FileDoesNotExistEx)

  writeToFile path content = do
    time <- liftIO $ getCurrentTime
    let mkFileAction = (\_ -> do
          fsContext <- ask
          (cur, fs) <- liftIO $ readIORef fsContext
          let normalized = normalize path
          validateName normalized
          liftIO . flip modifyIORef (\_ -> (cur, changeStructure (tail $ normalized) fs
                               (\folder name -> replaceFile folder name content time))) $ fsContext)
    fileActionOrError path mkFileAction (do
        fsContext <- ask
        (_, fs) <- liftIO $ readIORef fsContext
        let normalized = normalize path
        case findObject (safeInit $ tail $ normalized) fs of
          Just (Dir params) -> mkFileAction params
          _         -> throwM DirDoesNotExistEx)


  setCurDir ".." = do
    fsContext <- ask
    (cur, fs) <- liftIO $ readIORef fsContext
    case cur of
      "~"  -> throwM EndOfFileSystem
      _    -> do
        let parent = init $ normalize cur
        liftIO . flip modifyIORef (\_ -> (join parent, fs)) $ fsContext
  setCurDir path = do
    fsContext <- ask
    (_, fs) <- liftIO $ readIORef fsContext
    let normalized = normalize path
    liftIO . flip modifyIORef (\_ -> (join normalized, fs)) $ fsContext

  getDirFiles path = do
    fsContext <- ask
    (_, fs) <- liftIO $ readIORef fsContext
    let normalized = normalize path
    case findObject (tail $ normalized) fs of
      Just (Dir params) -> return . fmap getNames $ children params
      _         -> throwM DirDoesNotExistEx

  createDir path = do
    fsContext <- ask
    (cur, fs) <- liftIO $ readIORef fsContext
    let normalized = normalize path
    validateName normalized
    let newFs = changeStructure (tail $ normalized) fs
                       (\x dirName -> replaceFolder x (emptyDir dirName) (emptyDir dirName))
    liftIO . flip modifyIORef (\_ -> (cur, newFs)) $ fsContext

  receiveCanonicalizedPath path = return $ join $ normalize path

  receivePermissions path = receiveDirOrFileField filePermissions dirPermissions path

  receiveFileSize path = receiveDirOrFileField fileSize dirSize path

  receiveFileType = return . takeExtension

  receiveModTime path = fileActionOrError path (return . modifyTime)
                                               (throwM FileDoesNotExistEx)

  rmFile path = do
    let rmFileAction = (\_ -> do
          fsContext <- ask
          (cur, fs) <- liftIO $ readIORef fsContext
          let normalized = normalize path
          validateName normalized
          liftIO . flip modifyIORef (\_ -> (cur, changeStructure (tail $ normalized) fs
                                    (\folder name -> deleteFileOrDir folder name))) $ fsContext)
    fileActionOrError path rmFileAction (throwM FileDoesNotExistEx)

  rmDir path = do
    fsContext <- ask
    (cur, fs) <- liftIO $ readIORef fsContext
    let normalized = normalize path
    validateName normalized
    let newFs = changeStructure (tail $ normalized) fs (\x name -> deleteFileOrDir x name )
    liftIO . flip modifyIORef (\_ -> (cur, newFs)) $ fsContext

  findAllFiles pathes fileName = do
    fsContext <- ask
    (_, fs) <- liftIO $ readIORef fsContext
    let normalizedPathes =  ((++ [fileName]) . normalize) <$> pathes
    let mbFiles = (\path -> (findObject (tail path) fs) $> join path) <$> normalizedPathes

    return [x | Just x <- mbFiles]
