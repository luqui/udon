module Udon.External 
    ( ExtRef
    , External, extDump, extGC, extRead
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
import Data.Monoid

data ExtRef a = ExtRef Hash (Maybe a)

data External a 
    = External { extDump :: a -> Dump
               , extGC   :: a -> GCQueue
               , extRead :: Get a
               }

data Dump = Dump Put [(Hash, Dump)]

instance Monoid Dump where
    mempty = Dump (return ()) []
    mappend (Dump p xs) (Dump p' ys) = Dump (p >> p') (xs ++ ys)

newtype GCQueue = GCQueue [(Hash, Blob -> GCQueue)]

instance Monoid GCQueue where
    mempty = GCQueue []
    mappend (GCQueue xs) (GCQueue ys) = GCQueue (xs ++ ys)

pure   :: a -> External a
pure x = External {
    extDump = \_ -> mempty,
    extGC   = \_ -> mempty,
    extRead = return x }

sequ   :: (b -> a) -> External a -> (a -> External b) -> External b
sequ i pa j = External {
    extDump = \b -> let a = i b in extDump pa a `mappend` extDump (j a) b,
    extGC   = \b -> let a = i b in extGC   pa a `mappend` extGC   (j a) b,
    extRead = extRead pa >>= extRead . j }

ref    :: External a -> External (ExtRef a)
ref pa = External {
    extDump = \(ExtRef h v) -> Dump (put h) $ case v of
                                                Nothing -> []
                                                Just x  -> [(h, extDump pa x)],
    -- The extRead in this entry is why we cannot do typeclasses
    extGC   = \(ExtRef h v) -> GCQueue [(h, \blob -> extGC pa (runGet (extRead pa) blob))],
    extRead = fmap (\h -> ExtRef h Nothing) get }

binary :: Binary a => External a
binary = External {
    extDump = \a -> Dump (put a) [],
    extGC   = \_ -> mempty,
    extRead = get }
