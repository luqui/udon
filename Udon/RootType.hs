module Udon.RootType
    ( RootType, RootRef, makeRootType, typeCheck, makeRootRef )
where

import Udon.Hash
import Udon.DataDesc
import Data.Typeable
import Data.Binary
import Control.Monad (liftM2)


type RootTypeID = String
newtype RootType a = RootType RootTypeID

data RootRef
    = RootRef RootTypeID Hash

instance Binary RootRef where
    put (RootRef tid h) = put tid >> put h
    get = liftM2 RootRef get get

instance Data RootRef where
    desc = binary

makeRootType :: forall a. Typeable a => RootType a
makeRootType = RootType (show (typeOf (undefined :: a)))

typeCheck :: RootType a -> RootRef -> Maybe (ExtRef a)
typeCheck (RootType tid) (RootRef tid' h)
    | tid == tid' = Just (unsafeMakeExtRef h Nothing)
    | otherwise   = Nothing

-- This will lose any unstaged data in the ExtRef!  Careful!
makeRootRef :: RootType a -> ExtRef a -> RootRef
makeRootRef (RootType tid) ref = RootRef tid (extRefHash ref)
