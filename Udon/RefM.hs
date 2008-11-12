module Udon.RefM 
    ( RefM
    , unsafeLookup, remoteLookup
    )
where

import Udon.HashFS
import qualified Data.ByteString.Lazy as Str
import Control.Applicative
import Control.Monad
import Data.Binary
import Udon.RemoteRef

data RefM a 
    = Return a
    | Suspend [Ref] ([Str.ByteString] -> RefM a)

instance Functor RefM where
    fmap f (Return x) = Return (f x)
    fmap f (Suspend refs cont) = Suspend refs (fmap f . cont)

instance Applicative RefM where
    pure = Return
    Return f <*> Return x = Return (f x)
    Return f <*> Suspend refsx contx = Suspend refsx $ \d -> f <$> contx d
    Suspend refsf contf <*> Return x = Suspend refsf $ \d -> contf d <*> Return x
    Suspend refsf contf <*> Suspend refsx contx =
        Suspend (refsf ++ refsx) $ \d -> 
            let (d1,d2) = splitAt idx d in
            contf d1 <*> contx d2
        where
        idx = length refsf

instance Monad RefM where
    return = Return
    Return x >>= f = f x
    Suspend refs cont >>= f = Suspend refs $ cont >=> f

runRefM :: RefM a -> Either a ([Ref], [Str.ByteString] -> RefM a)
runRefM (Return x) = Left x
runRefM (Suspend rs cont) = Right (rs, cont)

runRefM_in_HashFS :: RefM a -> HashFS a
runRefM_in_HashFS (Return x) = return x
runRefM_in_HashFS (Suspend refs cont) = do
    blobs <- forM refs $ \r -> do
        blob <- getBlob r
        case blob of
            Nothing -> fail "Blob not found"
            Just b  -> return b
    runRefM_in_HashFS (cont blobs)

unsafeLookup :: (Binary b) => Ref -> RefM b
unsafeLookup ref = Suspend [ref] (\[d] -> Return (decode d))

remoteLookup :: (Binary b) => RemoteRef b -> RefM b
remoteLookup r = maybe (unsafeLookup (getRemoteRef r)) return (getRemoteVal r)
