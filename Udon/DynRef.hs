module Udon.DynRef 
    ( DynType, makeDynType
    , DynRef, dynRefToExtRef, unsafeExtRefToDynRef
    )
where

import Data.Typeable
import Udon.Hash
import Udon.DataDesc
import qualified Udon.DescCombinators as D
import Udon.DescInstances ()

type TypeID = String
newtype DynType a = DynType TypeID

-- Brittle!
makeDynType :: (Typeable a) => a -> DynType a
makeDynType = DynType . show . typeOf

data DynRef = DynRef TypeID Hash

instance Data DynRef where
    desc = D.wrap (uncurry DynRef, \(DynRef tid h) -> (tid,h)) desc

dynRefToExtRef :: DynType a -> DynRef -> Maybe (ExtRef a)
dynRefToExtRef (DynType tid) (DynRef tid' h) 
    | tid == tid' = Just $ unsafeMakeExtRef h Nothing
    | otherwise   = Nothing

-- unsafe because it forgets any data the ExtRef was storing
unsafeExtRefToDynRef :: DynType a -> ExtRef a -> DynRef
unsafeExtRefToDynRef (DynType tid) ref = DynRef tid (extRefHash ref)

