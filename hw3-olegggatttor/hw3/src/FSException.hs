module FSException
  ( FSException(..)
  ) where

import Control.Exception( Exception )

-- | FSException data
data FSException
  = DirDoesNotExistEx
  | FileDoesNotExistEx
  | NoSuchFileOrDirEx
  | EndOfFileSystem
  | InvalidName
  deriving Show

instance Exception FSException
