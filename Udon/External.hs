module Udon.External 
    ( Ext, deref, runExt )
where

import Udon.Hash
import Udon.Chunk
import Udon.Request
import Udon.DataDesc
import Udon.Database
import Control.Applicative
import Control.Monad
import Data.Binary (decode)


newtype Ext a = Ext (Request Hash Chunk a)
    deriving (Functor,Applicative,Monad)

deref :: (Data a) => ExtRef a -> Ext a
deref ref = Ext $ 
    case unsafeExtRefValue ref of
        Nothing -> runChunkGet (ddRead desc) <$> request (extRefHash ref)
        Just x  -> return x

runExt :: (Monad m) => Database m -> Ext a -> m a
runExt db (Ext req) = runRequestHandlerMaximal fetcher req
    where
    fetcher h = do
        r <- fetch db h
        case r of
            Nothing -> return Nothing
            Just r' -> liftM (Just . decode) r'
