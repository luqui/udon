module UdonShell.FSDB (fsdb, gcCollect) where

import Udon.DBAPI
import Udon.API
import qualified UdonShell.FST as FST
import qualified Data.Set as Set
import Control.Monad
import Debug.Trace

hashToPath h = ["objects", showHash h]

fsdb :: Database FST.FST
fsdb = Database {
    fetch = \h -> do
        let file = hashToPath h
        exists <- FST.checkFile file
        return $ if exists
                    then Just $ FST.readFile file
                    else Nothing,
    store = \h blob -> FST.newFile (hashToPath h) blob
  }

gcCollect :: [ExportRef] -> FST.FST ()
gcCollect rootset = do
    alive <- fmap (Set.map (last . hashToPath)) . markAlive fsdb . map exportRefHash $ rootset
    files <- FST.directoryFiles ["objects"]
    forM_ files $ \f -> do
        unless (f `Set.member` alive) $
            FST.deleteFile ["objects", f]
