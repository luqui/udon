module UdonShell.Pad 
    (Pad, insert, delete, lookup)
where

import Data.Typeable
import Prelude hiding (lookup)
import Udon.API
import qualified Udon.DescCombinators as D
import qualified Data.Map as Map

newtype Pad = Pad { unPad :: Map.Map String DynRef }
    deriving (Typeable)

conj f = Pad . f . unPad

instance Data Pad where
    desc = D.wrap (Pad . Map.fromDistinctAscList, Map.toList . unPad) desc

insert :: String -> DynRef -> Pad -> Pad
insert str ref = conj (Map.insert str ref)

delete :: String -> Pad -> Pad
delete str = conj (Map.delete str)

lookup :: Pad -> String -> Maybe DynRef
lookup (Pad pad) str = Map.lookup str pad
