module UdonShell.FSDB (fsdb) where

import Udon.DBAPI
import qualified UdonShell.FST as FST
import Debug.Trace

hashToPath h = [showHash h]

debug s = trace s $ return ()

fsdb :: Database FST.FST
fsdb = Database {
    fetch = \h -> do
        let file = hashToPath h
        exists <- FST.checkFile file
        debug $ "Request " ++ showHash h ++ "... " ++ (if exists then "found" else "not found")
        return $ if exists
                    then Just $ FST.readFile file
                    else Nothing,
    store = \h blob -> do
        debug $ "Store " ++ showHash h
        FST.newFile (hashToPath h) blob,
    export = \h -> do
        debug $ "Export " ++ showHash h
        return ()
  }


