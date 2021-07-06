module ToyFS
  ( toyFS
  ) where

import FileSystem
import System.Directory ( Permissions(..), emptyPermissions )

-- | Root for toy FS
toyFS
  :: FileSystem -- ^ Returns root
toyFS = Dir DirParams { dirPath = "~"
                      , dirPermissions = emptyPermissions{ readable = True, writable = True }
                      , dirSize = 0
                      , cntFiles = 0
                      , children = []
                      }
