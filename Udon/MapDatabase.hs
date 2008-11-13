module Udon.MapDatabase where

import Udon.Hash
import Udon.Database
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad

mapDatabase :: (MonadState (Map.Map Hash Blob) m) => Database m
mapDatabase = Database {
    fetch = \hash -> liftM (liftM return . Map.lookup hash) get,
    store = \hash blob -> modify (Map.insert hash blob)
  }


