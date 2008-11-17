{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Udon.Chunk 
    ( Chunk, chunkRefs
    , ChunkGet, liftGet, getHash, runChunkGet
    , ChunkPut, liftPut, putHash, runChunkPut
    )
where

import Udon.Hash
import Data.Binary
import Data.Binary.Builder (Builder, toLazyByteString)
import Data.Binary.Get (Get, runGet)
import Data.Binary.Put (PutM, unPut)
import qualified Control.Monad.State as State
import Control.Monad.Reader
import Control.Monad
import qualified Data.Map as Map
import Unsafe.Coerce

data Chunk = Chunk [Hash] Blob

getList :: Get a -> Get [a]
getList g = do
    len <- get :: Get Int
    replicateM len g

putList :: (a -> Put) -> [a] -> Put
putList f xs = put (length xs) >> mapM_ f xs

instance Binary Chunk where
    put (Chunk hs blob) = putList binHashPut hs >> put blob
    get = liftM2 Chunk (getList binHashGet) get

chunkRefs :: Chunk -> [Hash]
chunkRefs (Chunk hs _) = hs


newtype ChunkGet a = ChunkGet (ReaderT [Hash] Get a)
    deriving (Functor, Monad)

liftGet :: Get a -> ChunkGet a
liftGet = ChunkGet . lift

getHash :: ChunkGet Hash
getHash = ChunkGet $ liftM2 (!!) ask (lift get)

runChunkGet :: ChunkGet a -> Chunk -> a
runChunkGet (ChunkGet m) (Chunk hs blob) = runGet (runReaderT m hs) blob


newtype ChunkPut a = ChunkPut (State.StateT (Map.Map Int Hash) PutM a)
    deriving (Functor, Monad)

liftPut :: PutM a -> ChunkPut a
liftPut = ChunkPut . lift

putHash :: Hash -> ChunkPut ()
putHash h = ChunkPut $ do
    m <- State.get
    let idx | Map.null m = 0
            | otherwise  = fst (Map.findMax m) + 1
    State.put (Map.insert idx h m)
    lift $ put idx
    return ()

-- Emulating PairS because Data.Binary doesn't expose it,
-- nor any way to get from PutM a to a.
data PairS_OMGHAX a = PairS_OMGHAX a {-# UNPACK #-}!Builder

runPutM :: PutM a -> (a, Blob)
runPutM m = 
    let PairS_OMGHAX a builder = unsafeCoerce (unPut m) in
    (a, toLazyByteString builder)

runChunkPut :: ChunkPut a -> (a, Chunk)
runChunkPut (ChunkPut m) = 
    let ((a, rmap), blob) = runPutM (State.runStateT m (Map.empty))
        hashes = Map.elems rmap
    in (a, Chunk hashes blob)
