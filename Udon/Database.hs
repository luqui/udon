module Udon.Database where

import Udon.Hash
import Udon.DataDesc
import Data.Binary.Put (runPut)

-- This class is to guarantee uniqueness of descriptors
class Data a where
    desc :: DataDesc a

data Database m 
    = Database { fetch  :: Hash -> m (Maybe (m Blob))
               , store  :: Hash -> Blob -> m ()
               }

writeDump :: (Monad m) => Database m -> Dump -> m ()
writeDump db = \dump@(Dump put _) -> go (hashBlob (runPut put)) dump
    where
    go hash (Dump put subs) = do
        exists <- fetch db hash
        case exists of
            Just _  -> return ()
            Nothing -> do
                store db hash (runPut put)
                mapM_ (uncurry go) subs


