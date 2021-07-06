module CommandLineParser
  ( Instructions(..)
  , input
  ) where

import Options.Applicative ( info, (<**>), helper, command, argument, str, metavar, progDesc, idm
                           , subparser, many, ParserInfo, Mod, CommandFields )
import Data.Functor()
import Data.Semigroup()
import Control.Applicative()

-- | Commands for FileSystem data.
data Instructions
  = CD FilePath
  | LS FilePath
  | DIR
  | MKDIR FilePath
  | TOUCH FilePath
  | CAT FilePath
  | RM FilePath
  | WRITE FilePath [String]
  | SEARCH String
  | INFO FilePath
  | INVALID
  | EXIT

-- | Parses given string as commands
input
  :: ParserInfo Instructions  -- ^ Returns Parsed Instruction
input = info (  (subparser
             (  cdParser
             <> lsParser
             <> dirParser
             <> mkdirParser
             <> touchParser
             <> catParser
             <> rmParser
             <> writeParser
             <> searchParser
             <> infoParser
             <> exitParser))
             <**> helper) idm

-- | CD command parser
cdParser
  :: Mod CommandFields Instructions  -- ^ Returns parsed arguments for CD
cdParser = command "cd" (info (CD <$> argument str (metavar "PATH"))
                        (progDesc "Changes current directory to given path."))

-- | LS command parser
lsParser
  :: Mod CommandFields Instructions  -- ^ Returns parsed arguments for LS
lsParser = command "ls" (info (LS <$> argument str (metavar "PATH"))
                        (progDesc "Show files in given path directory."))

-- | DIR command parser
dirParser
  :: Mod CommandFields Instructions  -- ^ Returns parsed arguments for DIR
dirParser = command "dir" (info (pure DIR) (progDesc "Show files in current directory."))

-- | MKDIR command parser
mkdirParser
  :: Mod CommandFields Instructions  -- ^ Returns parsed arguments for MKDIR
mkdirParser = command "mkdir" (info (MKDIR <$> argument str (metavar "PATH"))
                              (progDesc "Creates directory at given path."))

-- | TOUCH command parser
touchParser
  :: Mod CommandFields Instructions  -- ^ Returns parsed arguments for TOUCH
touchParser = command "touch" (info (TOUCH <$> argument str (metavar "PATH"))
                              (progDesc "Creates file at given path."))

-- | CAT command parser
catParser
  :: Mod CommandFields Instructions  -- ^ Returns parsed arguments for CAT
catParser = command "cat" (info (CAT <$> argument str (metavar "FILEPATH"))
                          (progDesc "Shows file data."))

-- | RM command parser
rmParser
  :: Mod CommandFields Instructions  -- ^ Returns parsed arguments for RM
rmParser = command "rm" (info (RM <$> argument str (metavar "PATH"))
                        (progDesc "Removes directory or file at given path."))

-- | WRITE command parser
writeParser
  :: Mod CommandFields Instructions  -- ^ Returns parsed arguments for WRITE
writeParser = command "write" (info (WRITE <$> argument str (metavar "FILEPATH") <*>
                              many (argument str (metavar "TEXT")))
                              (progDesc "Writes text to file at given path."))

-- | SEARCH command parser
searchParser
  :: Mod CommandFields Instructions  -- ^ Returns parsed arguments for SEARCH
searchParser = command "search" (info (SEARCH <$> argument str (metavar "FILENAME"))
                                (progDesc "Finds all pathes to file with given name."))

-- | INFO command parser
infoParser
  :: Mod CommandFields Instructions  -- ^ Returns parsed arguments for INFO
infoParser =   command "info" (info (INFO <$> argument str (metavar "PATH"))
                              (progDesc "Shows dir or file info at given path."))

-- | EXIT command parser
exitParser
  :: Mod CommandFields Instructions  -- ^ Returns parsed arguments for EXIT
exitParser = command "exit" (info (pure EXIT) (progDesc "Terminates session."))
