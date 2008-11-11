module Udon.TypedBlob 
    ( TypedBlob
    , blobType, readBlob, makeBlob
    ) 
where

import Udon.Types
import qualified Data.ByteString.Lazy as Str
import Data.Binary
import Control.Applicative

data TypedBlob = TypedBlob Type Str.ByteString

instance Binary TypedBlob where
    get = TypedBlob <$> get <*> get
    put (TypedBlob t s) = put t >> put s

blobType :: TypedBlob -> Type
blobType (TypedBlob t _) = t

readBlob :: (Typed a) => TypedBlob -> Maybe a
readBlob (TypedBlob t s) = r
    where
    r = if t == getType result
            then Just result
            else Nothing
    result = decode s

makeBlob :: (Typed a) => a -> TypedBlob
makeBlob x = TypedBlob (getType x) (encode x)
