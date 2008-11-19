module Udon.DataDesc 
    ( ExtRef, extRefHash, unsafeExtRefValue, unsafeMakeExtRef, makeExtRef
    , DataDesc(..)
    , Data(..)
    , Dump(..), hashDump
    , pure, sequ, ref, binary
    )
where

import Udon.Hash
import Udon.Chunk
import Data.Binary
import Data.Maybe
import Data.Binary
import Data.Binary.Get (runGet)
import Data.Binary.Put (runPut)
import Data.Monoid

data ExtRef a = ExtRef Hash (Maybe a)

extRefHash :: ExtRef a -> Hash
extRefHash (ExtRef h _) = h

unsafeExtRefValue :: ExtRef a -> Maybe a
unsafeExtRefValue (ExtRef _ v) = v

unsafeMakeExtRef :: Hash -> Maybe a -> ExtRef a
unsafeMakeExtRef = ExtRef

makeExtRef :: (Data a) => a -> ExtRef a
makeExtRef x = unsafeMakeExtRef (hashDesc x) (Just x)

hashDesc :: (Data a) => a -> Hash
hashDesc = hashDump . ddDump desc

hashDump :: Dump -> Hash
hashDump (Dump rput _) = hashBinary . snd . runChunkPut $ rput

data DataDesc a 
    = DataDesc { ddDump :: a -> Dump
               , ddRead :: ChunkGet a
               }

-- This class is to guarantee uniqueness of descriptors
class Data a where
    desc :: DataDesc a


data Dump = Dump (ChunkPut ()) [(Hash, Dump)]

instance Monoid Dump where
    mempty = Dump (return ()) []
    mappend (Dump p xs) (Dump p' ys) = Dump (p >> p') (xs ++ ys)

pure   :: a -> DataDesc a
pure x = DataDesc {
    ddDump = \_ -> mempty,
    ddRead = return x }

sequ   :: (b -> a) -> DataDesc a -> (a -> DataDesc b) -> DataDesc b
sequ i pa j = DataDesc {
    ddDump = \b -> let a = i b in ddDump pa a `mappend` ddDump (j a) b,
    ddRead = ddRead pa >>= ddRead . j }

ref    :: DataDesc a -> DataDesc (ExtRef a)
ref pa = DataDesc {
    ddDump = \(ExtRef h v) -> Dump (putHash h) $ 
                                case v of
                                    Nothing -> []
                                    Just x  -> [(h, ddDump pa x)],
    -- The ddRead in this entry is why we cannot do typeclasses
    ddRead = fmap (\h -> ExtRef h Nothing) getHash }

binary :: Binary a => DataDesc a
binary = DataDesc {
    ddDump = \a -> Dump (liftPut $ put a) [],
    ddRead = liftGet get }

instance Data Hash where
    desc = DataDesc {
        ddDump = \h -> Dump (putHash h) [],
        ddRead = getHash }

instance Data a => Data (ExtRef a) where
    desc = ref desc
