module UdonShell.FSDB (fsdb) where

import Udon.DBAPI
import qualified UdonShell.FST as FST

hashToPath h = [showHash h]

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


