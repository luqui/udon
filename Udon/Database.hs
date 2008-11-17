module Udon.Database 
    ( Data(..)
    , Database(..)
    , writeData
    , makeDynRef
    , exportDyn
    )
where

import Udon.Hash
import Udon.Chunk
import Udon.DataDesc
import Udon.DynRef
import Data.Binary

data Database m 
    -- The weird signature for fetch is an optimization.  Sometimes
    -- it's easier to tell whether you *can* get a reference than
    -- it is to try to get it; e.g. in the local fsfs (a stat vs.
    -- an open).
    = Database { fetch  :: Hash -> m (Maybe (m Blob))
               , store  :: Hash -> Blob -> m ()
               }

newtype ExportRef = ExportRef Hash
    deriving (Eq, Ord)

instance Show ExportRef where
    show (ExportRef h) = "ExportRef \"" ++ showHash h ++ "\""

instance Binary ExportRef where
    put (ExportRef h) = binHashPut h
    get = fmap ExportRef binHashGet



writeChunk :: (Monad m) => Database m -> Chunk -> m ()
writeChunk db chunk = store db (hashBlob enc) enc
    where
    enc = encode chunk

writeDump :: (Monad m) => Database m -> Dump -> m Hash
writeDump db = \dump@(Dump rput _) -> do
    let hash = hashBinary . snd . runChunkPut $ rput
    go hash dump
    return hash
    where
    go hash (Dump rput subs) = do
        exists <- fetch db hash
        case exists of
            Just _  -> return ()
            Nothing -> do
                store db hash (encode . snd . runChunkPut $ rput)
                mapM_ (uncurry go) subs

writeData :: (Monad m, Data a) => Database m -> a -> m Hash
writeData db x = writeDump db (ddDump desc x)

-- Writes data in the extref to the database and returns the dynref.
-- Could be done better, bundling unwritten data with the dynref, as
-- is done with extref, but that has proven pretty tricky to do.
makeDynRef :: (Monad m, Data a) => Database m -> DynType a -> ExtRef a -> m DynRef
makeDynRef db dyntype ref = do
    hash <- writeData db (unsafeExtRefValue ref)
    return $ unsafeExtRefToDynRef dyntype ref

exportDyn :: (Monad m) => Database m -> DynRef -> m ExportRef
exportDyn db dynref = do
    -- This should mark something "exported" in the database, perhaps,
    -- so that it is treated as part of the root set for GC.
    hash <- writeData db dynref
    return (ExportRef hash)
