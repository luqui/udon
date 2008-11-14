module Udon.Database 
    ( Data(..)
    , Database(..)
    , writeRef
    )
where

import Udon.Hash
import Udon.DataDesc
import Data.Binary.Put (runPut)

data Database m 
    -- The weird signature for fetch is an optimization.  Sometimes
    -- it's easier to tell whether you *can* get a reference than
    -- it is to try to get it; e.g. in the local fsfs (a stat vs.
    -- an open).
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

writeRef :: (Data a, Monad m) => Database m -> ExtRef a -> m ()
writeRef db ref =
    case unsafeExtRefValue ref of
        Nothing -> return ()
        Just v -> writeDump db (ddDump desc v)
