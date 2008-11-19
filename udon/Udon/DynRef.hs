module Udon.DynRef 
    ( DynType, makeDynType
    , DynRef, dynRefToExtRef, extRefToDynRef
    )
where

import Data.Typeable
import Udon.Hash
import Udon.DataDesc
import qualified Udon.DescCombinators as D
import Udon.DescInstances ()
import Unsafe.Coerce
import Data.Monoid (mempty, mappend)

type Any = ()  -- like GHC.Any (i.e. safe to unsafeCoerce to)

type TypeID = String
newtype DynType a = DynType TypeID
    deriving (Typeable)

-- Brittle!
makeDynType :: (Typeable a) => a -> DynType a
makeDynType = DynType . show . typeOf

data DynRef = DynRef TypeID Hash (Maybe (Any, Dump))
    deriving Typeable

instance Data DynRef where
    desc = DataDesc { 
               ddDump = \ (DynRef tid h m) ->
                   let subdump = Dump (return ()) (maybe [] (\(_,d) -> [(h,d)]) m) in
                   ddDump subdesc (tid,h) `mappend` subdump,
               ddRead = fmap (\(tid,h) -> DynRef tid h Nothing) $ ddRead subdesc
           }
        where
        subdesc = desc :: DataDesc (TypeID, Hash)

dynRefToExtRef :: DynType a -> DynRef -> Maybe (ExtRef a)
dynRefToExtRef (DynType tid) (DynRef tid' h dat) 
    | tid == tid' = Just $ unsafeMakeExtRef h (fmap toExtDat dat)
    | otherwise   = Nothing
    where
    toExtDat (v,dump) = unsafeCoerce v

extRefToDynRef :: (Data a) => DynType a -> ExtRef a -> DynRef
extRefToDynRef (DynType tid) ref = DynRef tid (extRefHash ref) dat
    where
    dat = case unsafeExtRefValue ref of
              Nothing -> Nothing
              Just x -> Just (unsafeCoerce x, ddDump desc x)

