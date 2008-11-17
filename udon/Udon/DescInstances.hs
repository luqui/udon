module Udon.DescInstances where

import Udon.DataDesc
import qualified Udon.DescCombinators as D
import qualified Data.ByteString as Str
import qualified Data.ByteString.Lazy as StrL
import Data.Ratio
import qualified Data.IntSet as IntSet
import Data.Binary (Binary)
import qualified Data.Map as Map


-- A bunch of binary instances
instance Data Bool            where desc = binary
instance Data Char            where desc = binary
instance Data Double          where desc = binary
instance Data Float           where desc = binary
instance Data Int             where desc = binary
instance Data Integer         where desc = binary
instance Data Ordering        where desc = binary
instance Data ()              where desc = binary
instance Data Str.ByteString  where desc = binary
instance Data StrL.ByteString where desc = binary
instance Data IntSet.IntSet   where desc = binary

instance (Binary a, Integral a) => Data (Ratio a) where 
    desc = binary

-- Combinatory instances
instance Data a => Data [a] where desc = D.list desc
instance Data a => Data (Maybe a) where desc = D.descMaybe desc
instance (Data a, Data b) => Data (Either a b) where 
    desc = D.descEither desc desc
instance (Data a, Data b) => Data (a,b) where
    desc = D.pair desc desc

instance (Binary k, Data v) => Data (Map.Map k v) where
    desc = D.wrap (Map.fromDistinctAscList, Map.toAscList) $ D.list (D.pair binary desc)
