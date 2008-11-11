module Udon.RemoteRef 
    ( RemoteRef, makeRemote
    , getRemoteVal, getRemoteRef
    , lookupRemoteChg, lookupRemote
    )
where

import Udon.HashFS
import Data.Binary
import Control.Applicative
import Data.Maybe

data RemoteRef a
    = Local a Ref  -- Ref is a cache of the reference for this value
    | Remote Ref

instance Binary (RemoteRef a) where
    get = Remote <$> get
    put (Local _ r) = put r
    put (Remote r)  = put r

makeRemote :: (Binary a) => a -> RemoteRef a
makeRemote x = Local x (hashObject x)

getRemoteVal :: RemoteRef a -> Maybe a
getRemoteVal (Local x r) = Just x
getRemoteVal (Remote r)  = Nothing

getRemoteRef :: RemoteRef a -> Ref
getRemoteRef (Local x r) = r
getRemoteRef (Remote r)  = r

lookupRemoteChg :: (Binary a) => RemoteRef a -> HashFS (Maybe (RemoteRef a))
lookupRemoteChg (Local _ _) = return Nothing
lookupRemoteChg (Remote r)  = fmap (flip Local r) <$> getObject r

lookupRemote :: (Binary a) => RemoteRef a -> HashFS (RemoteRef a)
lookupRemote r = fromMaybe r <$> lookupRemoteChg r
