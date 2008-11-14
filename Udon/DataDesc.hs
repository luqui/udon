module Udon.DataDesc 
    ( ExtRef, extRefHash, unsafeExtRefValue, unsafeMakeExtRef, makeExtRef
    , DataDesc, ddDump, ddGC, ddRead
    , Data(..)
    , Dump(..)
    , GCQueue(..)
    , pure, sequ, ref, binary
    )
where

import Udon.Hash
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
hashDesc x = hashBlob (runPut put)
    where
    Dump put _ = ddDump desc x

data DataDesc a 
    = DataDesc { ddDump :: a -> Dump
               , ddGC   :: a -> GCQueue
               , ddRead :: Get a
               }

-- This class is to guarantee uniqueness of descriptors
class Data a where
    desc :: DataDesc a


data Dump = Dump Put [(Hash, Dump)]

instance Monoid Dump where
    mempty = Dump (return ()) []
    mappend (Dump p xs) (Dump p' ys) = Dump (p >> p') (xs ++ ys)

newtype GCQueue = GCQueue [(Hash, Blob -> GCQueue)]

instance Monoid GCQueue where
    mempty = GCQueue []
    mappend (GCQueue xs) (GCQueue ys) = GCQueue (xs ++ ys)

pure   :: a -> DataDesc a
pure x = DataDesc {
    ddDump = \_ -> mempty,
    ddGC   = \_ -> mempty,
    ddRead = return x }

sequ   :: (b -> a) -> DataDesc a -> (a -> DataDesc b) -> DataDesc b
sequ i pa j = DataDesc {
    ddDump = \b -> let a = i b in ddDump pa a `mappend` ddDump (j a) b,
    ddGC   = \b -> let a = i b in ddGC   pa a `mappend` ddGC   (j a) b,
    ddRead = ddRead pa >>= ddRead . j }

ref    :: DataDesc a -> DataDesc (ExtRef a)
ref pa = DataDesc {
    ddDump = \(ExtRef h v) -> Dump (put h) $ case v of
                                                Nothing -> []
                                                Just x  -> [(h, ddDump pa x)],
    -- The ddRead in this entry is why we cannot do typeclasses
    ddGC   = \(ExtRef h v) -> GCQueue [(h, \blob -> ddGC pa (runGet (ddRead pa) blob))],
    ddRead = fmap (\h -> ExtRef h Nothing) get }

binary :: Binary a => DataDesc a
binary = DataDesc {
    ddDump = \a -> Dump (put a) [],
    ddGC   = \_ -> mempty,
    ddRead = get }

instance Data a => Data (ExtRef a) where
    desc = ref desc
