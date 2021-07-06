{-# LANGUAGE ScopedTypeVariables #-}

module CommandLineInterface
  ( interactor
  ) where

import CommandLineParser ( Instructions(..), input )
import Options.Applicative ( handleParseResult, execParserPure, prefs, showHelpOnEmpty)
import FileSystem()
import System.IO.Error()
import System.IO
import Control.Exception ( Exception(..) )
import Control.Monad.Trans.Reader()
import Data.IORef()
import Control.Monad.Trans ( liftIO )
import Control.Monad.IO.Class ( MonadIO )
import Commands
import FSException()
import Control.Monad.Catch ( catch, MonadCatch(..) )
import System.Exit         (ExitCode)

-- | Invokes given command in given m
invokeCommand
  :: Commands m    -- ^ Given m (instance of Commands)
  => Instructions  -- ^ Given instruction to invoke
  -> m ()          -- ^ Returns m ()
invokeCommand (CD path)         = cd path
invokeCommand (LS path)         = ls path
invokeCommand DIR               = dir
invokeCommand (TOUCH path)      = touch path
invokeCommand (MKDIR path)      = mkdir path
invokeCommand (CAT path)        = cat path
invokeCommand (RM path)         = rm path
invokeCommand (WRITE path text) = write path (concat text)
invokeCommand (SEARCH path)     = find path
invokeCommand (INFO path)       = info path
invokeCommand EXIT              = logMsg "Terminating..."
invokeCommand INVALID           = return ()


-- | Parses actions from command line and perform action on geiven Command m
interactor
  :: (Exception e, MonadCatch m, Commands m, MonadIO m)  -- ^ Constraits for m and e
  => (e -> m ())                                         -- ^ Exceptions catcher
  -> m ()                                                -- ^ Returning value
interactor catcher = do
  cur <- getCurDir
  commandStr <- liftIO $ putStr cur >> putStr ">" >> hFlush stdout >>  getLine
  parseRes <- liftIO $ catch (handleParseResult $
                              execParserPure (prefs showHelpOnEmpty) input (words commandStr))
                              (\(_ :: ExitCode) -> return INVALID)
  case parseRes of
    EXIT -> invokeCommand EXIT
    _    -> catch (invokeCommand parseRes >> interactor catcher)
                  (\ex -> catcher ex >> interactor catcher)
