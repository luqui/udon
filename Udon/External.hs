module Udon.External 
    ( Ext, makeExtRef, deref, runExt )
where

import Udon.Hash
import Udon.Request
import Udon.DataDesc
import Udon.Database
import Data.Binary.Put (runPut)
import Data.Binary.Get (runGet)
import Control.Applicative
import Control.Monad


newtype Ext a = Ext (Request Hash Blob a)
    deriving (Functor,Applicative,Monad)

makeExtRef :: (Data a) => a -> ExtRef a 
makeExtRef x = unsafeMakeExtRef hash (Just x)
    where
    Dump putm _ = ddDump desc x
    hash = hashBlob $ runPut putm

deref :: (Data a) => ExtRef a -> Ext a
deref ref = Ext $ 
    case unsafeExtRefValue ref of
        Nothing -> runGet (ddRead desc) <$> request (extRefHash ref)
        Just x  -> return x

runExt :: (Monad m) => Database m -> Ext a -> m a
runExt db (Ext req) = runRequestHandlerMaximal fetcher req
    where
    fetcher h = do
        r <- fetch db h
        case r of
            Nothing -> return Nothing
            Just r' -> liftM Just r'