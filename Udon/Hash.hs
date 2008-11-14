module Udon.Hash 
    ( Blob, Hash, showHash, hashBlob, hashBinary ) 
where

import qualified Data.ByteString.Lazy as Str
import qualified Data.Digest.SHA256 as SHA
import Data.Binary
import qualified Codec.Binary.Base64 as Base64

type Blob = Str.ByteString

newtype Hash = Hash Blob
    deriving (Eq,Ord)

showHash :: Hash -> String
showHash (Hash blob) = Base64.encode (Str.unpack blob)

instance Binary Hash where
    put (Hash x) = put x
    get = fmap Hash get

hashBlob :: Blob -> Hash
hashBlob = Hash . Str.pack . SHA.hash . Str.unpack

hashBinary :: (Binary a) => a -> Hash
hashBinary = hashBlob . encode
