module Udon.Database 
    ( Data(..)
    , Database(..)
    )
where

import Udon.Hash
import Udon.Chunk
import Udon.DataDesc
import Data.Binary

data Database m 
    -- The weird signature for fetch is an optimization.  Sometimes
    -- it's easier to tell whether you *can* get a reference than
    -- it is to try to get it; e.g. in the local fsfs (a stat vs.
    -- an open).
    = Database { fetch  :: Hash -> m (Maybe (m Blob))
               , store  :: Hash -> Blob -> m ()
               }

writeChunk :: (Monad m) => Database m -> Chunk -> m ()
writeChunk db chunk = store db (hashBlob enc) enc
    where
    enc = encode chunk

writeDump :: (Monad m) => Database m -> Dump -> m ()
writeDump db = \dump@(Dump rput _) -> go (hashBinary . snd . runChunkPut $ rput) dump
    where
    go hash (Dump rput subs) = do
        exists <- fetch db hash
        case exists of
            Just _  -> return ()
            Nothing -> do
                store db hash (encode . snd . runChunkPut $ rput)
                mapM_ (uncurry go) subs
