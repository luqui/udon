module Udon.Database 
    ( Database(..)
    , writeData
    , exportDyn
    , readExportRef
    , markAlive
    )
where

import Udon.Hash
import Udon.Chunk
import Udon.DataDesc
import Udon.DynRef
import Data.Binary
import Control.Monad (liftM)
import qualified Data.Set as Set
import qualified Control.Monad.State as State

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


fetch' :: (Monad m) => Database m -> Hash -> m (Maybe Blob)
fetch' db hash = 
    maybe (return Nothing) (liftM Just) =<< fetch db hash

writeChunk :: (Monad m) => Database m -> Chunk -> m ()
writeChunk db chunk = store db (hashBlob enc) enc
    where
    enc = encode chunk

writeDump :: (Monad m) => Database m -> Dump -> m Hash
writeDump db = \dump -> do
    let hash = hashDump dump
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

getRefs :: (Monad m) => Database m -> Hash -> m [Hash]
getRefs db h = do
    getter <- fetch db h
    case getter of
        Nothing -> return []
        Just g -> do
            blob <- g
            return $ chunkRefs (decode blob)

markAlive :: (Monad m) => Database m -> [Hash] -> m (Set.Set Hash)
markAlive db hashes = State.execStateT (mapM_ go hashes) Set.empty
    where
    go h = do
        seen <- State.get
        if h `Set.member` seen
            then return ()
            else do
                State.put (Set.insert h seen)
                nexts <- State.lift $ getRefs db h
                mapM_ go nexts
    

writeData :: (Monad m, Data a) => Database m -> a -> m Hash
writeData db x = writeDump db (ddDump desc x)

exportDyn :: (Monad m) => Database m -> DynRef -> m ExportRef
exportDyn db dynref = do
    -- This should mark something "exported" in the database, perhaps,
    -- so that it is treated as part of the root set for GC.
    hash <- writeData db dynref
    return (ExportRef hash)

readExportRef :: (Monad m) => Database m -> ExportRef -> m (Maybe DynRef)
readExportRef db (ExportRef hash) = 
    (liftM.liftM) (runChunkGet (ddRead desc) . decode) $ fetch' db hash
